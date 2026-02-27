{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Thanks to our dynamic 'Opaleye' effect, we can write an alternative interpreter which,
as well as performing SQL operations as before, will also keep a tally of the number of
SQL operations (SELECTs, INSERTs etc) that have been performed. This is really useful for debugging.

The intended use-case is a sort of benchmark that runs several Opaleye operations for different
"sizes", counts the SQL operations, and prints the tallies to the console. This lets us detect if
some datbase operations are ineffecient.

For example, suppose our model has users with @UserId@s; those users an have multiple @Transaction@s, which
are composed of multiple @SubTransaction@s etc.
To insert a group of new users, we would need to insert the users, insert the transactions, and insert the subtransactions.
Ideally, the number of @INSERT@s should not depend on the number of @User@s or the number or size of their @Transactions@.
We would expect the number of SELECTs to remain basically constant (O(1)), while the execution time might grow linearly (O(u * t * s)).

A very naive implementation might be:

@
insertUsersNaive :: ('Opaleye' :> es) => [User] -> Eff es ()
insertUsersNaive users = for_ users $ \user -> do
  insertUserFlat user
  for (transactions user) $ \transaction -> do
    insertTransactionFlat transaction
    for (subTransactions transaction) $ \subTransaction -> do
      insertSubTransactionFlat subTransaction
@

However, if we ran a "benchmark" that looked something like this:

@
u1, u5, u10, u50 :: [User]
u1 = [User {transactions = [Transaction [SubTransaction]]}] -- one user, one transaction, one sub-transaction
u5 = ...  -- five users, each with five transactions, each with 5 sub-transactions

benchmark :: ('Opaleye' :> es, State SQLOperationCounts :> es, IOE :> es) => Eff es ()
benchmark = for_ [(1, u1), (5, u5), (10, u10), (50, u50)] $ \(n, users) -> do
  (counts, ()) <- withCounts $ insertUsersNaive users
  liftIO . putStrLn $ "Counts at n=" <> show n <> ": " <> 'renderCountsBrief' counts

main :: IO ()
main = runEff . 'Conn.runWithConnectInfo' connInfo . evalState @SQLOperationCounts mempty . runOpaleyeWithConnectionCounting $ benchmark
  where
    connInfo = ...
@

We will probably see something like:

@
Counts at n=1: INSERT: 3
Counts at n=5: INSERT: 155
Counts at n=10: INSERT: 1110
Counts at n=50: INSERT: 127550
@

This is obviously going to have a severe performance impact. Rearranging our implementatino of @insertUsers@:

@
insertUsersBetter :: ('Opaleye' :> es) => [User] -> Eff es ()
insertUsersBetter users = do
  let transactions_ = concatMap transactions users
      subTransactions_ = concatMap subTransactions transactions_
  insertUsersFlat users
  insertTransactionsFlat transactions_
  insertSubTransactionsFlat subTransactions_
@

As long as @insertTransactionsFlat@ etc are smart enough to only do one 'runInsert', then we should now get:

@
Counts at n=1: INSERT: 3
Counts at n=5: INSERT: 3
Counts at n=10: INSERT: 3
Counts at n=50: INSERT: 3
@

Note that we used 'renderCountsBrief' for simplicity. If we wanted to debug in more detail, we could have used
'renderCounts' instead:

@
Counts at n=1: INSERT: user: 1
                       transaction: 1
                       sub_transaction: 1
Counts at n=5: INSERT: user: 5
                       transaction: 25
                       sub_transaction: 125
Counts at n=10: INSERT: user: 10
                        transaction: 100
                        sub_transaction: 1000
Counts at n=50: INSERT: user: 50
                        transaction: 2500
                        sub_transaction: 125000
@
-}
module Effectful.Opaleye.Count
  ( -- * Counting SQL operations
    opaleyeAddCounting
  , withCounts
  , module PostgreSQL.Count
  )
where

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text as T
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Opaleye.Effect
import Effectful.State.Static.Shared
import Numeric.Natural
import qualified Opaleye as O
import qualified Opaleye.Internal.PrimQuery as O (TableIdentifier (..))
import qualified Opaleye.Internal.Table as O
import PostgreSQL.Count
#if !MIN_VERSION_effectful_core(2,5,1)
import Control.Monad (when)
import Effectful.Dispatch.Static
import Effectful.Internal.Env
import Effectful.Internal.Monad
import GHC.Stack
#endif

------------------------------------------------------------
-- Tallying SQL operations

{- | Add counting of SQL operations to the interpreter of an 'Opaleye' effect.
Note that the effect itself is not actually interpreted. We do this using 'passthrough',
which lets us perform some actions based on the 'Opaleye' constructor and then pass them
through to the upstream handler (e.g. 'Effectful.Opaleye.runOpaleyeWithConnection' or
'Effectful.Opaleye.runOpaleyeConnection'). See 'Effectful.Opaleye.runOpaleyeConnectionCounting'
and 'Effectful.Opaleye.runOpaleyeWithConnectionCounting' for interpreters that do both.

Note: this function should only be used once, otherwise the operations will be tallied
more than once. Unless you're sure, it's probably better to use
'Effectful.Opaleye.runOpaleyeConnectionCounting' or
'Effectful.Opaleye.runOpaleyeWithConnectionCounting'.
-}
opaleyeAddCounting ::
  forall a es.
  (HasCallStack, State SQLOperationCounts :> es) =>
  Eff (Opaleye : es) a ->
  Eff (Opaleye : es) a
opaleyeAddCounting = interpose $ \env op -> do
  incrementOp op
  passthrough env op
  where
    incrementOp :: forall b localEs. Opaleye (Eff localEs) b -> Eff (Opaleye : es) ()
    incrementOp = \case
      RunSelectExplicit {} -> incrementSelect
      RunSelectFoldExplicit {} -> incrementSelect
      RunInsert ins -> incrementInsert $ insertTableName ins
      RunDelete del -> incrementDelete $ deleteTableName del
      RunUpdate upd -> incrementUpdate $ updateTableName upd

    incrementMap :: TableName -> Map TableName Natural -> Map TableName Natural
    incrementMap = Map.alter (Just . maybe 1 succ)

    incrementSelect = modify $ \counts ->
      counts {sqlSelects = succ $ sqlSelects counts}
    incrementInsert name = modify $ \counts ->
      counts {sqlInserts = incrementMap name $ sqlInserts counts}
    incrementUpdate name = modify $ \counts ->
      counts {sqlUpdates = incrementMap name $ sqlUpdates counts}
    incrementDelete name = modify $ \counts ->
      counts {sqlDeletes = incrementMap name $ sqlDeletes counts}

-- | This allows us to count the number of SQL operations over the course of a sub-operation.
withCounts ::
  (State SQLOperationCounts :> es) =>
  Eff es a ->
  Eff es (SQLOperationCounts, a)
withCounts eff = do
  countsBefore <- get
  res <- eff
  countsAfter <- get
  pure (countsAfter `subtractCounts` countsBefore, res)

#if !MIN_VERSION_effectful_core(2,5,1)
-- passthrough was only added in effectful-core-2.5.1, so if we don't have access to a version
-- after that then we have to replicate it here
passthrough ::
  (HasCallStack, DispatchOf e ~ Dynamic, e :> es, e :> localEs) =>
  LocalEnv localEs handlerEs ->
  e (Eff localEs) a ->
  Eff es a
passthrough (LocalEnv les) op = unsafeEff $ \es -> do
  Handler handlerEs handler <- getEnv es
  when (envStorage les /= envStorage handlerEs) $ do
    error "les and handlerEs point to different Storages"
  unEff (withFrozenCallStack handler (LocalEnv les) op) handlerEs
{-# NOINLINE passthrough #-}
#endif

------------------------------------------------------------
-- Getting table identifiers from opaleye operations

tableIdentifierToTableName :: O.TableIdentifier -> TableName
tableIdentifierToTableName (O.TableIdentifier mSchema table) =
  TableName (T.pack <$> mSchema) (T.pack table)

insertTableName :: O.Insert haskells -> TableName
insertTableName (O.Insert table _ _ _) =
  tableIdentifierToTableName . O.tableIdentifier $ table

updateTableName :: O.Update haskells -> TableName
updateTableName (O.Update table _ _ _) =
  tableIdentifierToTableName . O.tableIdentifier $ table

deleteTableName :: O.Delete haskells -> TableName
deleteTableName (O.Delete table _ _) =
  tableIdentifierToTableName . O.tableIdentifier $ table

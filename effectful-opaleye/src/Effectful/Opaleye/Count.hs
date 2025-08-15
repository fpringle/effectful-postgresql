{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
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
    SQLOperationCounts (..)
  , opaleyeAddCounting
  , withCounts

    -- * Pretty-printing
  , printCounts
  , printCountsBrief
  , renderCounts
  , renderCountsBrief
  , prettyCounts
  , prettyCountsBrief
  )
where

import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, mapMaybe)
import qualified Data.Text as T
import Database.PostgreSQL.Simple.Types (QualifiedIdentifier (..))
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Opaleye.Effect
import Effectful.State.Static.Shared
import GHC.Generics
import Numeric.Natural
import qualified Opaleye as O
import qualified Opaleye.Internal.PrimQuery as O (TableIdentifier (..))
import qualified Opaleye.Internal.Table as O
import qualified Text.PrettyPrint as P
import qualified Text.PrettyPrint.HughesPJClass as P
#if !MIN_VERSION_effectful_core(2,5,1)
import Control.Monad (when)
import Effectful.Dispatch.Static
import Effectful.Internal.Env
import Effectful.Internal.Monad
import GHC.Stack
#endif

------------------------------------------------------------
-- Tallying SQL operations

{- | This tracks the number of SQL operations that have been performed in the
'Opaleye' effect, along with which table it was performed on (where possible).

@INSERT@, @DELETE@ and @UPDATE@ operations act on one table only, so we can tally the number
of each that are performed on each table (indexed by a t'QualifiedIdentifier').
@SELECT@ operations can act on multiple tables, so we just track the total number of selects.

If required, t'SQLOperationCounts' can be constructed using 'Monoid' and combined using 'Semigroup'.

We use non-negative 'Natural's as a tally since a negative number of operations makes no sense.
-}
data SQLOperationCounts = SQLOperationCounts
  { sqlSelects :: Natural
  , sqlInserts :: Map QualifiedIdentifier Natural
  , sqlDeletes :: Map QualifiedIdentifier Natural
  , sqlUpdates :: Map QualifiedIdentifier Natural
  }
  deriving (Show, Eq, Generic)

instance Semigroup SQLOperationCounts where
  SQLOperationCounts s1 i1 d1 u1 <> SQLOperationCounts s2 i2 d2 u2 =
    SQLOperationCounts
      (s1 + s2)
      (i1 `addNatMaps` i2)
      (d1 `addNatMaps` d2)
      (u1 `addNatMaps` u2)
    where
      addNatMaps = Map.unionWith (+)

instance Monoid SQLOperationCounts where
  mempty = SQLOperationCounts 0 mempty mempty mempty

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

    incrementMap :: QualifiedIdentifier -> Map QualifiedIdentifier Natural -> Map QualifiedIdentifier Natural
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

subtractNat :: Natural -> Natural -> Natural
a `subtractNat` b = if a > b then a - b else 0

subtractNatMaps :: (Ord k) => Map k Natural -> Map k Natural -> Map k Natural
subtractNatMaps c1 c2 =
  let f op count = Map.adjust (`subtractNat` count) op
  in  Map.foldrWithKey f c1 c2

subtractCounts :: SQLOperationCounts -> SQLOperationCounts -> SQLOperationCounts
subtractCounts (SQLOperationCounts s1 i1 d1 u1) (SQLOperationCounts s2 i2 d2 u2) =
  SQLOperationCounts
    (s1 `subtractNat` s2)
    (i1 `subtractNatMaps` i2)
    (d1 `subtractNatMaps` d2)
    (u1 `subtractNatMaps` u2)

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

tableIdentifierToQualifiedIdentifier :: O.TableIdentifier -> QualifiedIdentifier
tableIdentifierToQualifiedIdentifier (O.TableIdentifier mSchema table) =
  QualifiedIdentifier (T.pack <$> mSchema) (T.pack table)

insertTableName :: O.Insert haskells -> QualifiedIdentifier
insertTableName (O.Insert table _ _ _) =
  tableIdentifierToQualifiedIdentifier . O.tableIdentifier $ table

updateTableName :: O.Update haskells -> QualifiedIdentifier
updateTableName (O.Update table _ _ _) =
  tableIdentifierToQualifiedIdentifier . O.tableIdentifier $ table

deleteTableName :: O.Delete haskells -> QualifiedIdentifier
deleteTableName (O.Delete table _ _) =
  tableIdentifierToQualifiedIdentifier . O.tableIdentifier $ table

------------------------------------------------------------
-- Pretty rendering and printing counts

instance P.Pretty SQLOperationCounts where
  pPrint = prettyCounts

{- | Print an t'SQLOperationCounts' to stdout using 'prettyCounts'.
For less verbose output, see 'printCountsBrief'.
-}
printCounts :: (MonadIO m) => SQLOperationCounts -> m ()
printCounts = liftIO . putStrLn . renderCounts

{- | Print an t'SQLOperationCounts' to stdout using 'prettyCountsBrief'.
For more verbose output, see 'printCounts'.
-}
printCountsBrief :: (MonadIO m) => SQLOperationCounts -> m ()
printCountsBrief = liftIO . putStrLn . renderCountsBrief

{- | Render an t'SQLOperationCounts' using 'prettyCounts'.
For less verbose output, see 'renderCountsBrief'.

For more control over how the 'P.Doc' gets rendered, use 'P.renderStyle' with a custom 'P.style'.
-}
renderCounts :: SQLOperationCounts -> String
renderCounts = P.render . prettyCounts

{- | Render an t'SQLOperationCounts' using 'prettyCountsBrief'.
For more verbose output, see 'renderCounts'.

For more control over how the 'P.Doc' gets rendered, use 'P.renderStyle' with a custom 'P.style'.
-}
renderCountsBrief :: SQLOperationCounts -> String
renderCountsBrief = P.render . prettyCountsBrief

{- | Pretty-print an t'SQLOperationCounts' using "Text.PrettyPrint".
For each 'Map', we'll print one line for each table. For less verbose output,
see 'prettyCountsBrief'.

This is also the implementation of 'P.pPrint' for t'SQLOperationCounts'.
-}
prettyCounts :: SQLOperationCounts -> P.Doc
prettyCounts = prettyCountsWith $ \mp ->
  let counts = Map.toList mp
      renderPair (name, count) = prefix (renderTableName name) <$> renderNat count
  in  fmap (P.vcat . NE.toList) . NE.nonEmpty $ mapMaybe renderPair counts

{- | Pretty-print an t'SQLOperationCounts' using "Text.PrettyPrint".
For each 'Map', we'll print just the sum of the counts. For more verbose output,
see 'prettyCounts'.
-}
prettyCountsBrief :: SQLOperationCounts -> P.Doc
prettyCountsBrief = prettyCountsWith $ \mp ->
  let total = sum $ Map.elems mp
  in  renderNat total

prettyCountsWith :: (Map QualifiedIdentifier Natural -> Maybe P.Doc) -> SQLOperationCounts -> P.Doc
prettyCountsWith renderMap (SQLOperationCounts selects inserts deletes updates) =
  let parts =
        catMaybes
          [ prefix "SELECT" <$> renderNat selects
          , prefix "INSERT" <$> renderMap inserts
          , prefix "UPDATE" <$> renderMap updates
          , prefix "DELETE" <$> renderMap deletes
          ]
  in  case parts of
        [] -> "None"
        _ -> P.vcat parts

prefix :: P.Doc -> P.Doc -> P.Doc
prefix t n = t P.<> ":" P.<+> n

renderNat :: Natural -> Maybe P.Doc
renderNat = \case
  0 -> Nothing
  n -> Just $ P.pPrint @Integer $ toInteger n

renderTableName :: QualifiedIdentifier -> P.Doc
renderTableName (QualifiedIdentifier mSchema table) =
  case mSchema of
    Nothing -> renderText table
    Just schema -> renderText schema <> "." <> renderText table

renderText :: T.Text -> P.Doc
renderText = P.text . T.unpack

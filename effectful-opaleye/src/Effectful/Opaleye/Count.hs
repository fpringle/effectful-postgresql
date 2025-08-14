{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Effectful.Opaleye.Count
  ( -- * Counting SQL operations
    SQLOperationCounts (..)
  , opaleyeAddCounting
  , withCounts
  )
where

import Data.Map (Map)
import qualified Data.Map as Map
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

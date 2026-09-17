{-# LANGUAGE CPP #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TemplateHaskell #-}

module Effectful.PostgreSQL.Effect
  ( -- * Effect
    PostgreSQL (..)

    -- ** Interpreters
  , runPostgreSQL
#if OTEL
  , runPostgreSQLOT
#endif

    -- * Lifted versions of functions from Database.PostgreSQL.Simple

    -- ** Queries that return results
  , query
  , query_
  , queryWith
  , queryWith_

    -- ** Statements that do not return results
  , execute
  , execute_
  , executeMany

    -- ** Transaction handling
  , withTransaction
  , withTransactionLevel
  , PSQL.IsolationLevel (..)
  , withTransactionMode
  , PSQL.TransactionMode (..)
  , PSQL.ReadWriteMode (..)
  , withTransactionModeRetry
  , withTransactionModeRetry'
  , withTransactionSerializable
  , withTransactionSerialisable
  , withSavepoint
  , begin
  , commit
  , rollback

    -- ** Queries that stream results
  , fold
  , foldWithOptions
  , fold_
  , foldWithOptions_
  , forEach
  , forEach_
  , returning
  , foldWith
  , foldWithOptionsAndParser
  , foldWith_
  , foldWithOptionsAndParser_
  , forEachWith
  , forEachWith_
  , returningWith
  )
where

import qualified Control.Exception as E
import Data.Int (Int64)
import qualified Database.PostgreSQL.Simple as PSQL
import qualified Database.PostgreSQL.Simple.Transaction as PSQL
import qualified Database.PostgreSQL.Simple.FromRow as PSQL
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.PostgreSQL.Connection
import Effectful.TH
#if OTEL
import qualified "hs-opentelemetry-instrumentation-postgresql-simple" OpenTelemetry.Instrumentation.PostgresqlSimple as OT
#endif

-- | Dynamic effect representing all the Postgres operations we want to perform.
data PostgreSQL :: Effect where
  -- | Lifted 'PSQL.query'.
  Query :: (PSQL.ToRow q, PSQL.FromRow r) => PSQL.Query -> q -> PostgreSQL m [r]
  -- | Lifted 'PSQL.queryWith'.
  QueryWith :: (PSQL.ToRow q) => PSQL.RowParser r -> PSQL.Query -> q -> PostgreSQL m [r]
  -- | Lifted 'PSQL.query_'.
  Query_ :: (PSQL.FromRow r) => PSQL.Query -> PostgreSQL m [r]
  -- | Lifted 'PSQL.queryWith_'.
  QueryWith_ :: PSQL.RowParser r -> PSQL.Query -> PostgreSQL m [r]
  --

  -- | Lifted 'PSQL.execute'.
  Execute :: (PSQL.ToRow q) => PSQL.Query -> q -> PostgreSQL m Int64
  -- | Lifted 'PSQL.execute_'.
  Execute_ :: PSQL.Query -> PostgreSQL m Int64
  -- | Lifted 'PSQL.executeMany'.
  ExecuteMany :: (PSQL.ToRow q) => PSQL.Query -> [q] -> PostgreSQL m Int64
  --

  -- | Lifted 'PSQL.withTransaction'.
  WithTransaction :: m a -> PostgreSQL m a
  -- | Lifted 'PSQL.withTransactionLevel'.
  WithTransactionLevel :: PSQL.IsolationLevel -> m a -> PostgreSQL m a
  -- | Lifted 'PSQL.withTransactionMode'.
  WithTransactionMode :: PSQL.TransactionMode -> m a -> PostgreSQL m a
  -- | Lifted 'PSQL.withTransactionModeRetry'.
  WithTransactionModeRetry :: PSQL.TransactionMode -> (PSQL.SqlError -> Bool) -> m a -> PostgreSQL m a
  -- | Lifted 'PSQL.withTransactionModeRetry''.
  WithTransactionModeRetry' :: E.Exception e => PSQL.TransactionMode -> (e -> Bool) -> m a -> PostgreSQL m a
  -- | Lifted 'PSQL.withTransactionSerializable'.
  WithTransactionSerializable :: m a -> PostgreSQL m a
  -- | Lifted 'PSQL.withSavepoint'.
  WithSavepoint :: m a -> PostgreSQL m a
  -- | Lifted 'PSQL.begin'.
  Begin :: PostgreSQL m ()
  -- | Lifted 'PSQL.commit'.
  Commit :: PostgreSQL m ()
  -- | Lifted 'PSQL.rollback'.
  Rollback :: PostgreSQL m ()
  --

  -- | Lifted 'PSQL.fold'.
  Fold ::
    (PSQL.FromRow row, PSQL.ToRow params) =>
    PSQL.Query ->
    params ->
    a ->
    (a -> row -> m a) ->
    PostgreSQL m a
  -- | Lifted 'PSQL.fold_'.
  Fold_ ::
    (PSQL.FromRow row) =>
    PSQL.Query ->
    a ->
    (a -> row -> m a) ->
    PostgreSQL m a
  -- | Lifted 'PSQL.foldWithOptions'.
  FoldWithOptions ::
    (PSQL.FromRow row, PSQL.ToRow params) =>
    PSQL.FoldOptions ->
    PSQL.Query ->
    params ->
    a ->
    (a -> row -> m a) ->
    PostgreSQL m a
  -- | Lifted 'PSQL.foldWithOptions_'.
  FoldWithOptions_ ::
    (PSQL.FromRow row) =>
    PSQL.FoldOptions ->
    PSQL.Query ->
    a ->
    (a -> row -> m a) ->
    PostgreSQL m a
  -- | Lifted 'PSQL.forEach'.
  ForEach ::
    (PSQL.FromRow r, PSQL.ToRow q) =>
    PSQL.Query ->
    q ->
    (r -> m ()) ->
    PostgreSQL m ()
  -- | Lifted 'PSQL.forEach_'.
  ForEach_ ::
    (PSQL.FromRow r) =>
    PSQL.Query ->
    (r -> m ()) ->
    PostgreSQL m ()
  -- | Lifted 'PSQL.returning'.
  Returning :: (PSQL.ToRow q, PSQL.FromRow r) => PSQL.Query -> [q] -> PostgreSQL m [r]
  -- | Lifted 'PSQL.foldWith'.
  FoldWith ::
    (PSQL.ToRow params) =>
    PSQL.RowParser row ->
    PSQL.Query ->
    params ->
    a ->
    (a -> row -> m a) ->
    PostgreSQL m a
  -- | Lifted 'PSQL.foldWithOptionsAndParser'.
  FoldWithOptionsAndParser ::
    (PSQL.ToRow params) =>
    PSQL.FoldOptions ->
    PSQL.RowParser row ->
    PSQL.Query ->
    params ->
    a ->
    (a -> row -> m a) ->
    PostgreSQL m a
  -- | Lifted 'PSQL.foldWith_'.
  FoldWith_ ::
    () =>
    PSQL.RowParser row ->
    PSQL.Query ->
    a ->
    (a -> row -> m a) ->
    PostgreSQL m a
  -- | Lifted 'PSQL.foldWithOptionsAndParser_'.
  FoldWithOptionsAndParser_ ::
    () =>
    PSQL.FoldOptions ->
    PSQL.RowParser row ->
    PSQL.Query ->
    a ->
    (a -> row -> m a) ->
    PostgreSQL m a
  -- | Lifted 'PSQL.forEachWith'.
  ForEachWith ::
    (PSQL.ToRow q) =>
    PSQL.RowParser r ->
    PSQL.Query ->
    q ->
    (r -> m ()) ->
    PostgreSQL m ()
  -- | Lifted 'PSQL.forEachWith_'.
  ForEachWith_ ::
    () =>
    PSQL.RowParser r ->
    PSQL.Query ->
    (r -> m ()) ->
    PostgreSQL m ()
  -- | Lifted 'PSQL.returningWith'.
  ReturningWith :: (PSQL.ToRow q) => PSQL.RowParser r -> PSQL.Query -> [q] -> PostgreSQL m [r]

makeEffect ''PostgreSQL

-- | British alias of 'withTransactionSerializable'.
withTransactionSerialisable ::
  (HasCallStack, PostgreSQL :> es) => Eff es a -> Eff es a
withTransactionSerialisable = withTransactionSerializable
{-# INLINE withTransactionSerialisable #-}

localUnliftWithConn ::
  (HasCallStack, WithConnection :> es, IOE :> es) =>
  LocalEnv localEs es ->
  (PSQL.Connection -> (forall b. Eff localEs b -> IO b) -> IO a) ->
  Eff es a
localUnliftWithConn env f =
  withConnection $ \conn ->
    localSeqUnliftIO env $ \unlift ->
      liftIO $ f conn unlift
{-# INLINE localUnliftWithConn #-}

(...) :: (a -> b) -> (t1 -> t2 -> a) -> t1 -> t2 -> b
unlift ... f = \a' row -> unlift $ f a' row

{- | Obvious interepreter for 'PostgreSQL'. Just gets a 'PSQL.Connection' from 'WithConnection' and calls the
corresponding function from "Database.PostgreSQL.Simple".
-}
runPostgreSQL :: forall es a. (HasCallStack, WithConnection :> es, IOE :> es) => Eff (PostgreSQL : es) a -> Eff es a
runPostgreSQL = interpret $ \env -> \case
  Query q row ->
    withConnection $ \conn -> liftIO (PSQL.query conn q row)
  QueryWith parser q row ->
    withConnection $ \conn -> liftIO (PSQL.queryWith parser conn q row)
  Query_ row ->
    withConnection $ \conn -> liftIO (PSQL.query_ conn row)
  QueryWith_ parser row ->
    withConnection $ \conn -> liftIO (PSQL.queryWith_ parser conn row)
  Execute q row -> withConnection $ \conn -> liftIO (PSQL.execute conn q row)
  Execute_ q -> withConnection $ \conn -> liftIO (PSQL.execute_ conn q)
  ExecuteMany q row -> withConnection $ \conn -> liftIO (PSQL.executeMany conn q row)
  WithTransaction f -> localUnliftWithConn env $ \conn unlift -> PSQL.withTransaction conn (unlift f)
  WithTransactionLevel level f -> localUnliftWithConn env $ \conn unlift -> PSQL.withTransactionLevel level conn (unlift f)
  WithTransactionMode mode f -> localUnliftWithConn env $ \conn unlift -> PSQL.withTransactionMode mode conn (unlift f)
  WithTransactionModeRetry mode shouldRetry f -> localUnliftWithConn env $ \conn unlift -> PSQL.withTransactionModeRetry mode shouldRetry conn (unlift f)
  WithTransactionModeRetry' mode shouldRetry f -> localUnliftWithConn env $ \conn unlift -> PSQL.withTransactionModeRetry' mode shouldRetry conn (unlift f)
  WithTransactionSerializable f -> localUnliftWithConn env $ \conn unlift -> PSQL.withTransactionSerializable conn (unlift f)
  WithSavepoint f -> localUnliftWithConn env $ \conn unlift -> PSQL.withSavepoint conn (unlift f)
  Begin -> withConnection $ liftIO . PSQL.begin
  Commit -> withConnection $ liftIO . PSQL.commit
  Rollback -> withConnection $ liftIO . PSQL.rollback
  Fold q params a f ->
    localUnliftWithConn env $ \conn unlift ->
      PSQL.fold conn q params a (unlift ... f)
  Fold_ q a f ->
    localUnliftWithConn env $ \conn unlift ->
      PSQL.fold_ conn q a (unlift ... f)
  FoldWithOptions opts q params a f ->
    localUnliftWithConn env $ \conn unlift ->
      PSQL.foldWithOptions opts conn q params a (unlift ... f)
  FoldWithOptions_ opts q a f ->
    localUnliftWithConn env $ \conn unlift ->
      PSQL.foldWithOptions_ opts conn q a (unlift ... f)
  ForEach q row forR ->
    localUnliftWithConn env $ \conn unlift ->
      PSQL.forEach conn q row (unlift . forR)
  ForEach_ q forR ->
    localUnliftWithConn env $ \conn unlift ->
      PSQL.forEach_ conn q (unlift . forR)
  Returning q rows -> withConnection $ \conn -> liftIO $ PSQL.returning conn q rows
  FoldWith parser q params a f ->
    localUnliftWithConn env $ \conn unlift ->
      PSQL.foldWith parser conn q params a (unlift ... f)
  FoldWithOptionsAndParser opts parser q params a f ->
    localUnliftWithConn env $ \conn unlift ->
      PSQL.foldWithOptionsAndParser opts parser conn q params a (unlift ... f)
  FoldWith_ parser q a f ->
    localUnliftWithConn env $ \conn unlift ->
      PSQL.foldWith_ parser conn q a (unlift ... f)
  FoldWithOptionsAndParser_ opts parser q a f ->
    localUnliftWithConn env $ \conn unlift ->
      PSQL.foldWithOptionsAndParser_ opts parser conn q a (unlift ... f)
  ForEachWith parser q row forR ->
    localUnliftWithConn env $ \conn unlift ->
      PSQL.forEachWith parser conn q row (unlift . forR)
  ForEachWith_ parser q forR ->
    localUnliftWithConn env $ \conn unlift ->
      PSQL.forEachWith_ parser conn q (unlift . forR)
  ReturningWith parser q rows -> withConnection $ \conn -> liftIO $ PSQL.returningWith parser conn q rows

#if OTEL
{- | An interpreter for the 'PostgreSQL' effect that runs database operations using OpenTelemetry instrumentation.

Basically the same as 'runPostgreSQL' except it uses the functions from
[OpenTelemetry.Instrumentation.PostgresqlSimple](https://hackage-content.haskell.org/package/hs-opentelemetry-instrumentation-postgresql-simple/docs/OpenTelemetry-Instrumentation-PostgresqlSimple.html).

Note that the @enable-opentel@ cabal flag must be set to enable this functionality.
-}
runPostgreSQLOT :: forall es a. (HasCallStack, WithConnection :> es, IOE :> es) => Eff (PostgreSQL : es) a -> Eff es a
runPostgreSQLOT = interpret $ \env -> \case
  Query q row ->
    withConnection $ \conn -> OT.query conn q row
  QueryWith parser q row ->
    withConnection $ \conn -> OT.queryWith parser conn q row
  Query_ row ->
    withConnection $ \conn -> OT.query_ conn row
  QueryWith_ parser row ->
    withConnection $ \conn -> OT.queryWith_ parser conn row
  Execute q row -> withConnection $ \conn -> OT.execute conn q row
  Execute_ q -> withConnection $ \conn -> OT.execute_ conn q
  ExecuteMany q row -> withConnection $ \conn -> OT.executeMany conn q row
  WithTransaction f -> localUnliftWithConn env $ \conn unlift -> OT.withTransaction conn (unlift f)
  WithTransactionLevel level f -> localUnliftWithConn env $ \conn unlift -> PSQL.withTransactionLevel level conn (unlift f)
  WithTransactionMode mode f -> localUnliftWithConn env $ \conn unlift -> PSQL.withTransactionMode mode conn (unlift f)
  WithTransactionModeRetry mode shouldRetry f -> localUnliftWithConn env $ \conn unlift -> PSQL.withTransactionModeRetry mode shouldRetry conn (unlift f)
  WithTransactionModeRetry' mode shouldRetry f -> localUnliftWithConn env $ \conn unlift -> PSQL.withTransactionModeRetry' mode shouldRetry conn (unlift f)
  WithTransactionSerializable f -> localUnliftWithConn env $ \conn unlift -> PSQL.withTransactionSerializable conn (unlift f)
  WithSavepoint f -> localUnliftWithConn env $ \conn unlift -> OT.withSavepoint conn (unlift f)
  Begin -> withConnection $ liftIO . OT.begin
  Commit -> withConnection $ liftIO . OT.commit
  Rollback -> withConnection $ liftIO . OT.rollback
  Fold q params a f ->
    localUnliftWithConn env $ \conn unlift ->
      OT.fold conn q params a (unlift ... f)
  Fold_ q a f ->
    localUnliftWithConn env $ \conn unlift ->
      OT.fold_ conn q a (unlift ... f)
  FoldWithOptions opts q params a f ->
    localUnliftWithConn env $ \conn unlift ->
      OT.foldWithOptions opts conn q params a (unlift ... f)
  FoldWithOptions_ opts q a f ->
    localUnliftWithConn env $ \conn unlift ->
      OT.foldWithOptions_ opts conn q a (unlift ... f)
  ForEach q row forR ->
    localUnliftWithConn env $ \conn unlift ->
      OT.forEachWith PSQL.fromRow conn q row (unlift . forR)
  ForEach_ q forR ->
    localUnliftWithConn env $ \conn unlift ->
      OT.forEach_ conn q (unlift . forR)
  Returning q rows -> withConnection $ \conn -> OT.returning conn q rows
  FoldWith parser q params a f ->
    localUnliftWithConn env $ \conn unlift ->
      OT.foldWith parser conn q params a (unlift ... f)
  FoldWithOptionsAndParser opts parser q params a f ->
    localUnliftWithConn env $ \conn unlift ->
      OT.foldWithOptionsAndParser opts parser conn q params a (unlift ... f)
  FoldWith_ parser q a f ->
    localUnliftWithConn env $ \conn unlift ->
      OT.foldWith_ parser conn q a (unlift ... f)
  FoldWithOptionsAndParser_ opts parser q a f ->
    localUnliftWithConn env $ \conn unlift ->
      OT.foldWithOptionsAndParser_ opts parser conn q a (unlift ... f)
  ForEachWith parser q row forR ->
    localUnliftWithConn env $ \conn unlift ->
      OT.forEachWith parser conn q row (unlift . forR)
  ForEachWith_ parser q forR ->
    localUnliftWithConn env $ \conn unlift ->
      OT.forEachWith_ parser conn q (unlift . forR)
  ReturningWith parser q rows -> withConnection $ \conn -> OT.returningWith parser conn q rows
#endif

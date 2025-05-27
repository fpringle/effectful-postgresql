{-# LANGUAGE CPP #-}

module Effectful.PostgreSQL
  ( -- * Effect
    WithConnection (..)
  , withConnection

    -- ** Interpreters
  , runWithConnection

#if POOL
  , runWithConnectionPool
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

import Data.Int (Int64)
import qualified Database.PostgreSQL.Simple as PSQL
import qualified Database.PostgreSQL.Simple.FromRow as PSQL
import Effectful
import Effectful.PostgreSQL.Connection as Conn
import GHC.Stack
#if POOL
import Effectful.PostgreSQL.Connection.Pool as Pool
#endif

-- | Lifted 'PSQL.query'.
query ::
  (HasCallStack, WithConnection :> es, IOE :> es, PSQL.ToRow q, PSQL.FromRow r) =>
  PSQL.Query ->
  q ->
  Eff es [r]
query q row = withConnection $ \conn -> liftIO (PSQL.query conn q row)

-- | Lifted 'PSQL.query_'.
query_ ::
  (HasCallStack, WithConnection :> es, IOE :> es, PSQL.FromRow r) =>
  PSQL.Query ->
  Eff es [r]
query_ row = withConnection $ \conn -> liftIO (PSQL.query_ conn row)

-- | Lifted 'PSQL.queryWith'.
queryWith ::
  (HasCallStack, WithConnection :> es, IOE :> es, PSQL.ToRow q) =>
  PSQL.RowParser r ->
  PSQL.Query ->
  q ->
  Eff es [r]
queryWith parser q row =
  withConnection $ \conn -> liftIO (PSQL.queryWith parser conn q row)

-- | Lifted 'PSQL.queryWith_'.
queryWith_ ::
  (HasCallStack, WithConnection :> es, IOE :> es) =>
  PSQL.RowParser r ->
  PSQL.Query ->
  Eff es [r]
queryWith_ parser row =
  withConnection $ \conn -> liftIO (PSQL.queryWith_ parser conn row)

-- | Lifted 'PSQL.execute'.
execute ::
  (HasCallStack, WithConnection :> es, IOE :> es, PSQL.ToRow q) =>
  PSQL.Query ->
  q ->
  Eff es Int64
execute q row = withConnection $ \conn -> liftIO (PSQL.execute conn q row)

-- | Lifted 'PSQL.execute_'.
execute_ ::
  (HasCallStack, WithConnection :> es, IOE :> es) =>
  PSQL.Query ->
  Eff es Int64
execute_ row = withConnection $ \conn -> liftIO (PSQL.execute_ conn row)

-- | Lifted 'PSQL.executeMany'.
executeMany ::
  (HasCallStack, WithConnection :> es, IOE :> es, PSQL.ToRow q) =>
  PSQL.Query ->
  [q] ->
  Eff es Int64
executeMany q rows = withConnection $ \conn -> liftIO (PSQL.executeMany conn q rows)

-- | Lifted 'PSQL.withTransaction'.
withTransaction ::
  (HasCallStack, WithConnection :> es, IOE :> es) => Eff es a -> Eff es a
withTransaction f =
  unliftWithConn $ \conn unlift ->
    PSQL.withTransaction conn (unlift f)

-- | Lifted 'PSQL.withSavepoint'.
withSavepoint :: (HasCallStack, WithConnection :> es, IOE :> es) => Eff es a -> Eff es a
withSavepoint f =
  unliftWithConn $ \conn unlift ->
    PSQL.withSavepoint conn (unlift f)

-- | Lifted 'PSQL.begin'.
begin :: (HasCallStack, WithConnection :> es, IOE :> es) => Eff es ()
begin = withConnection $ liftIO . PSQL.begin

-- | Lifted 'PSQL.commit'.
commit :: (HasCallStack, WithConnection :> es, IOE :> es) => Eff es ()
commit = withConnection $ liftIO . PSQL.commit

-- | Lifted 'PSQL.rollback'.
rollback :: (HasCallStack, WithConnection :> es, IOE :> es) => Eff es ()
rollback = withConnection $ liftIO . PSQL.rollback

(...) :: (a -> b) -> (t1 -> t2 -> a) -> t1 -> t2 -> b
unlift ... f = \a' row -> unlift $ f a' row

unliftWithConn ::
  (HasCallStack, WithConnection :> es, IOE :> es) =>
  (PSQL.Connection -> (forall b. Eff es b -> IO b) -> IO a) ->
  Eff es a
unliftWithConn f =
  withConnection $ \conn ->
    withRunInIO $ \unlift ->
      liftIO $ f conn unlift
{-# INLINE unliftWithConn #-}

-- | Lifted 'PSQL.fold'.
fold ::
  (HasCallStack, WithConnection :> es, IOE :> es, PSQL.FromRow row, PSQL.ToRow params) =>
  PSQL.Query ->
  params ->
  a ->
  (a -> row -> Eff es a) ->
  Eff es a
fold q params a f =
  unliftWithConn $ \conn unlift ->
    PSQL.fold conn q params a (unlift ... f)

-- | Lifted 'PSQL.foldWithOptions'.
foldWithOptions ::
  (HasCallStack, WithConnection :> es, IOE :> es, PSQL.FromRow row, PSQL.ToRow params) =>
  PSQL.FoldOptions ->
  PSQL.Query ->
  params ->
  a ->
  (a -> row -> Eff es a) ->
  Eff es a
foldWithOptions opts q params a f =
  unliftWithConn $ \conn unlift ->
    PSQL.foldWithOptions opts conn q params a (unlift ... f)

-- | Lifted 'PSQL.fold_'.
fold_ ::
  (HasCallStack, WithConnection :> es, IOE :> es, PSQL.FromRow row) =>
  PSQL.Query ->
  a ->
  (a -> row -> Eff es a) ->
  Eff es a
fold_ q a f =
  unliftWithConn $ \conn unlift ->
    PSQL.fold_ conn q a (unlift ... f)

-- | Lifted 'PSQL.foldWithOptions_'.
foldWithOptions_ ::
  (HasCallStack, WithConnection :> es, IOE :> es, PSQL.FromRow row) =>
  PSQL.FoldOptions ->
  PSQL.Query ->
  a ->
  (a -> row -> Eff es a) ->
  Eff es a
foldWithOptions_ opts q a f =
  unliftWithConn $ \conn unlift ->
    PSQL.foldWithOptions_ opts conn q a (unlift ... f)

-- | Lifted 'PSQL.forEach'.
forEach ::
  (HasCallStack, WithConnection :> es, IOE :> es, PSQL.FromRow r, PSQL.ToRow q) =>
  PSQL.Query ->
  q ->
  (r -> Eff es ()) ->
  Eff es ()
forEach q row forR =
  unliftWithConn $ \conn unlift ->
    PSQL.forEach conn q row (unlift . forR)

-- | Lifted 'PSQL.forEach_'.
forEach_ ::
  (HasCallStack, WithConnection :> es, IOE :> es, PSQL.FromRow r) =>
  PSQL.Query ->
  (r -> Eff es ()) ->
  Eff es ()
forEach_ q forR =
  unliftWithConn $ \conn unlift ->
    PSQL.forEach_ conn q (unlift . forR)

-- | Lifted 'PSQL.returning'.
returning ::
  (HasCallStack, WithConnection :> es, IOE :> es, PSQL.ToRow q, PSQL.FromRow r) =>
  PSQL.Query ->
  [q] ->
  Eff es [r]
returning q rows = withConnection $ \conn -> liftIO $ PSQL.returning conn q rows

-- | Lifted 'PSQL.foldWith'.
foldWith ::
  (HasCallStack, WithConnection :> es, IOE :> es, PSQL.ToRow params) =>
  PSQL.RowParser row ->
  PSQL.Query ->
  params ->
  a ->
  (a -> row -> Eff es a) ->
  Eff es a
foldWith parser q params a f =
  unliftWithConn $ \conn unlift ->
    PSQL.foldWith parser conn q params a (unlift ... f)

-- | Lifted 'PSQL.foldWithOptionsAndParser'.
foldWithOptionsAndParser ::
  (HasCallStack, WithConnection :> es, IOE :> es, PSQL.ToRow params) =>
  PSQL.FoldOptions ->
  PSQL.RowParser row ->
  PSQL.Query ->
  params ->
  a ->
  (a -> row -> Eff es a) ->
  Eff es a
foldWithOptionsAndParser opts parser q params a f =
  unliftWithConn $ \conn unlift ->
    PSQL.foldWithOptionsAndParser opts parser conn q params a (unlift ... f)

-- | Lifted 'PSQL.foldWith_'.
foldWith_ ::
  (HasCallStack, WithConnection :> es, IOE :> es) =>
  PSQL.RowParser row ->
  PSQL.Query ->
  a ->
  (a -> row -> Eff es a) ->
  Eff es a
foldWith_ parser q a f =
  unliftWithConn $ \conn unlift ->
    PSQL.foldWith_ parser conn q a (unlift ... f)

-- | Lifted 'PSQL.foldWithOptionsAndParser_'.
foldWithOptionsAndParser_ ::
  (HasCallStack, WithConnection :> es, IOE :> es) =>
  PSQL.FoldOptions ->
  PSQL.RowParser row ->
  PSQL.Query ->
  a ->
  (a -> row -> Eff es a) ->
  Eff es a
foldWithOptionsAndParser_ opts parser q a f =
  unliftWithConn $ \conn unlift ->
    PSQL.foldWithOptionsAndParser_ opts parser conn q a (unlift ... f)

-- | Lifted 'PSQL.forEachWith'.
forEachWith ::
  (HasCallStack, WithConnection :> es, IOE :> es, PSQL.ToRow q) =>
  PSQL.RowParser r ->
  PSQL.Query ->
  q ->
  (r -> Eff es ()) ->
  Eff es ()
forEachWith parser q row forR =
  unliftWithConn $ \conn unlift ->
    PSQL.forEachWith parser conn q row (unlift . forR)

-- | Lifted 'PSQL.forEachWith_'.
forEachWith_ ::
  (HasCallStack, WithConnection :> es, IOE :> es) =>
  PSQL.RowParser r ->
  PSQL.Query ->
  (r -> Eff es ()) ->
  Eff es ()
forEachWith_ parser row forR =
  unliftWithConn $ \conn unlift ->
    PSQL.forEachWith_ parser conn row (unlift . forR)

-- | Lifted 'PSQL.returningWith'.
returningWith ::
  (HasCallStack, WithConnection :> es, IOE :> es, PSQL.ToRow q) =>
  PSQL.RowParser r ->
  PSQL.Query ->
  [q] ->
  Eff es [r]
returningWith parser q rows =
  withConnection $ \conn -> liftIO $ PSQL.returningWith parser conn q rows

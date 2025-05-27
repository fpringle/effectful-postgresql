module Effectful.PostgreSQL.Connection.Pool
  ( -- * Interpret with a Connection pool
    runWithConnectionPool

    -- * Re-export
  , module Pool
  )
where

import qualified Database.PostgreSQL.Simple as PSQL
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.PostgreSQL.Connection
import UnliftIO.Pool as Pool

{- | Rather than keeping one connection alive and re-using it for the whole
process, we might want to create a 'Pool' of connections and only "ask" for
one when we need it. This function uses "UnliftIO.Pool" to do just that.
-}
runWithConnectionPool ::
  (HasCallStack, IOE :> es) =>
  Pool.Pool PSQL.Connection ->
  Eff (WithConnection : es) a ->
  Eff es a
runWithConnectionPool pool = interpret $ \env -> \case
  WithConnection f ->
    localSeqUnlift env $ \unlift -> do
      Pool.withResource pool $ unlift . f

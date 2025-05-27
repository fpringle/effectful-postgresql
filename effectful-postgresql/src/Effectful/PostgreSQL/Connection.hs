{-# LANGUAGE TemplateHaskell #-}

module Effectful.PostgreSQL.Connection
  ( -- * Effect
    WithConnection (..)
  , withConnection

    -- * Interpret with a single Connection
  , runWithConnection
  )
where

import qualified Database.PostgreSQL.Simple as PSQL
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.TH

{- | A dynamic effect that lets us use a database 'PSQL.Connection',
without specifying __how__ that connection is supplied.

For example, we might want to just provide a connection and let the interpreter
use that for the whole procedure (see 'Effectful.PostgreSQL.Connection.runWithConnection').

Or, we might want to create a connection pool which provides connections when
the interpreter asks for them (see "Effectful.PostgreSQL.Connection.Pool").
-}
data WithConnection :: Effect where
  -- | Use a 'PSQL.Connection' provided by an interpreter.
  WithConnection :: (PSQL.Connection -> m a) -> WithConnection m a

makeEffect ''WithConnection

{- | Run a t'WithConnection' effect by simply supplying a 'PSQL.Connection'.
The connection will be kept alive for the whole duration of the procedure,
which might not be want you want for long-running processes. If so, see
"Effectful.PostgreSQL.Connection.Pool".
-}
runWithConnection ::
  (HasCallStack) => PSQL.Connection -> Eff (WithConnection : es) a -> Eff es a
runWithConnection conn = interpret $ \env -> \case
  WithConnection f ->
    localSeqUnlift env $ \unlift -> unlift $ f conn

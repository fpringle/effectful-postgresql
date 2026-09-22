{-# LANGUAGE CPP #-}

module Effectful.PostgreSQL
  ( -- * Effects
    WithConnection
  , withConnection
  , PostgreSQL

    -- ** Interpreters
  , runWithConnection

#if POOL
  , runWithConnectionPool
#endif

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

  -- ** Error predicates
  , PSQL.isSerializationError
  , PSQL.isNoActiveTransactionError
  , PSQL.isFailedTransactionError
  )
where

import qualified Database.PostgreSQL.Simple.Transaction as PSQL
import Effectful.PostgreSQL.Connection as Conn
import Effectful.PostgreSQL.Effect
#if POOL
import Effectful.PostgreSQL.Connection.Pool as Pool
#endif

# effectful-postgresql

This package provides an `effectful` effect for [postgresql-simple](https://hackage.haskell.org/package/postgresql-simple)'s `Connection` type.

It defines a dynamic effect to allow effectful functions to use a `Connection`, without worrying about where that `Connection` comes from.

For a higher-level effect library using [Opaleye](https://hackage.haskell.org/package/opaleye), see [effectful-opaleye](../effectful-opaleye#readme).

## Effectful functions

In the `WithConnection` effect we can always request a `Connection` and use it as we normally
would:

```haskell
import Effectful.PostgreSQL as EP
import qualified Database.PostgreSQL.Simple as PSQL

insertAndList :: (EP.WithConnection :> es, IOE :> es) => Eff es [User]
insertAndList = EP.withConnection $ \conn -> do
  PSQL.execute conn "insert into users (first_name) values (?)" ["Nuala"]
  PSQL.query conn "select * from users where first_name in ?" $ Only $ In ["Anna", "Boris", "Carla"]
```

In fact, for convenience we also define lifted versions of all of the query/execute
functions from `postgresql-simple`, so we can completely forget about `Connection` and rewrite the above to:

```haskell

import Effectful.PostgreSQL

insertAndList :: (EP.WithConnection :> es, IOE :> es) => Eff es [User]
insertAndList = do
  EP.execute "insert into users (first_name) values (?)" ["Nuala"]
  EP.query "select * from users where first_name in ?" $ Only $ In ["Anna", "Boris", "Carla"]
```

The same goes for other functions:

```haskell
-- use a transaction
insertAndListCarefully :: (EP.WithConnection :> es, IOE :> es) => Eff es [User]
insertAndListCarefully = EP.withTransaction insertAndList

-- stream + fold over results (in Eff)
countUsersIneffeciently :: (EP.WithConnection :> es, IOE :> es, Log :> es) => Eff es Int
countUsersIneffeciently =
  EP.fold_ "select * from users" 0 $ \acc row ->
    log $ "User: " <> show row
    pure $ acc + 1
```

## Interpreters

The simplest way of running the `WithConnection` effect is by just providing a `Connection`, which we can get in the normal ways:

```haskell
import Effectful.PostgreSQL as EP
import qualified Database.PostgreSQL.Simple as PSQL

usingConnection :: IO ()
usingConnection =
  bracket (PSQL.connectPostgreSQL "") PSQL.close $ \conn ->
    runEff . EP.runWithconnection conn $ insertAndListCarefully

usingConnectInfo :: IO ()
usingConnectInfo =
    runEff . EP.runWithconnectInfo PSQL.defaultConnectInfo $ insertAndListCarefully
```

Alternatively, we can use a connection pool (from [resource-pool](https://hackage.haskell.org/package/resource-pool)
and [unliftio-pool](https://hackage.haskell.org/package/unliftio-pool)), which is much better suited to
long-running processes like servers.

```haskell
import Effectful.PostgreSQL as EP
import qualified Database.PostgreSQL.Simple as PSQL
import qualified UnliftIO.Pool as P

usingConnectionPool :: IO ()
usingConnectionPool = do
  poolCfg <- P.mkDefaultPoolConfig (PSQL.connectPostgreSQL "") PSQL.close 5.0 10
  pool <- P.newPool poolCfg
  runEff . EP.runWithconnectionPool pool $ insertAndListCarefully
```

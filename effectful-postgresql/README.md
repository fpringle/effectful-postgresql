# effectful-postgresql

This package provides `effectful` effects for using [postgresql-simple](https://hackage.haskell.org/package/postgresql-simple)'s `Connection` type.

It defines:
- a dynamic `WithConnection` effect to allow effectful functions to use a `Connection`, without worrying about where that `Connection` comes from.
- a dynamic `PostgreSQL` effect ro run database operations from `postgresql-simple`.

For a higher-level effect library using [Opaleye](https://hackage.haskell.org/package/opaleye), see [effectful-opaleye](https://github.com/fpringle/effectful-postgresql/blob/main/effectful-opaleye#readme).

## Effectful functions

In the `WithConnection` effect we can always request a `Connection` and use it as we normally
would:

```haskell
import Effectful.PostgreSQL as EP
import qualified Database.PostgreSQL.Simple as PSQL

insertAndList :: (WithConnection :> es, IOE :> es) => Eff es [User]
insertAndList = EP.withConnection $ \conn -> do
  liftIO $ PSQL.execute conn "insert into users (first_name) values (?)" ["Nuala"]
  liftIO $ PSQL.query conn "select * from users where first_name in ?" $ Only $ In ["Anna", "Boris", "Carla"]
```

The `PostgreSQL` effect lets us completely forget about `Connection` and rewrite the above to:

```haskell

import Effectful.PostgreSQL

insertAndList :: (PostgreSQL :> es) => Eff es [User]
insertAndList = do
  EP.execute "insert into users (first_name) values (?)" ["Nuala"]
  EP.query "select * from users where first_name in ?" $ Only $ In ["Anna", "Boris", "Carla"]
```

The same goes for other functions:

```haskell
-- use a transaction
insertAndListCarefully :: (PostgreSQL :> es) => Eff es [User]
insertAndListCarefully = EP.withTransaction insertAndList

-- stream + fold over results (in Eff)
countUsersIneffeciently :: (PostgreSQL :> es, Log :> es) => Eff es Int
countUsersIneffeciently =
  EP.fold_ "select * from users" 0 $ \acc row -> do
    log $ "User: " <> show row
    pure $ acc + 1
```

## Interpreters

In order to discharge the `PostgreSQL` effect we use the `WithConnection` effect:

```haskell
dischargePostgreSQL :: (WithConnection :> es, IOE :> es) => Eff es [User]
dischargePostgreSQL = runPostgreSQL insertAndListCarefully
```

Alternatively we can use the OpenTelemetry support provided by [hs-opentelemetry-instrumentation-postgresql-simple](https://hackage-content.haskell.org/package/hs-opentelemetry-instrumentation-postgresql-simple/docs/OpenTelemetry-Instrumentation-PostgresqlSimple.html) (note that this requires enabling the `enable-opentel` cabal flag):

```haskell
dischargePostgreSQLUsingOpenTelemetry :: (WithConnection :> es, IOE :> es) => Eff es [User]
dischargePostgreSQLUsingOpenTelemetry = runPostgreSQLOT insertAndListCarefully
```

The simplest way of running the `WithConnection` effect is by just providing a `Connection`, which we can get in the normal ways:

```haskell
import Effectful.PostgreSQL as EP
import qualified Database.PostgreSQL.Simple as PSQL

usingConnection :: IO ()
usingConnection =
  void $ bracket (PSQL.connectPostgreSQL "") PSQL.close $ \conn ->
    runEff . runWithConnection conn $ runPostgreSQL insertAndListCarefully

usingConnectInfo :: IO ()
usingConnectInfo =
    void . runEff . runWithConnectInfo PSQL.defaultConnectInfo $ runPostgreSQL insertAndListCarefully
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
  void . runEff . runWithConnectionPool pool $ runPostgreSQL insertAndListCarefully
```

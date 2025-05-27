# effectful-opaleye

This package provides an `effectful` effect for [Opaleye](https://hackage.haskell.org/package/opaleye) operations.

It combines the very safe, high-level syntax of Opaleye, with the `WithConnection` abstraction of [effectful-postgresql](../effectful-postgresql#readme).

## Effectful functions

In the `Opaleye` effect we can perform the 4 main operations permitted by Opaleye: query, insert, delete, and update.

```haskell
{-# LANGUAGE Arrows #-}
import Control.Arrow
import Effectful.Opaleye as EO
import qualified Opaleye as O

insertAndList :: (EO.Opaleye :> es) => Eff es [User]
insertAndList = do
  EO.runInsert $ O.Insert userTable [User {firstName = "Nuala"}] O.rCount Nothing

  EO.runDelete $ O.Delete userTable isAdmin O.rCount

  EO.runUpdate $ O.Update userTable (\user -> user {updatedAt = O.now}) isAdmin O.rCount

  EO.runSelect $ proc () -> do
    user <- O.selectTable userTable -< ()
    O.restrict -< firstName user `O.in_` (O.toFields <$> ["Anna", "Boris", "Carla"])
    returnA -< user
```

## Interpreters

To run the `Opaleye` effect we can use the `WithConnection` effect from [effectful-postgresql](../effectful-postgresql#readme):

```haskell
import Effectful.PostgreSQL as EP
import Effectful.Opaleye as EO

doOpaleyeStuff :: (WithConnection :> es) => Eff es [User]
doOpaleyeStuff = EO.runOpaleyeWithConnection insertAndList
```

The `WithConnection` effect can then be dispatched using one of its [interpreters](../effectful-postgresql#interpreters).
Or, to skip that entirely, we can just use `runOpaleyeConnection`:

```haskell
doOpaleyeStuff :: PSQL.Connection -> Eff es [User]
doOpaleyeStuff conn = EO.runOpaleyeConnection conn insertAndList
```

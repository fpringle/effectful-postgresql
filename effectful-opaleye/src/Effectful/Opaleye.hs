{-# LANGUAGE TemplateHaskell #-}

module Effectful.Opaleye
  ( Opaleye (..)
  , runSelect
  , runSelectI
  , runSelectExplicit
  , runSelectFold
  , runSelectFoldExplicit
  , runInsert
  , runDelete
  , runUpdate
  , runOpaleyeWithConnection
  , runOpaleyeConnection
  )
where

import Data.Profunctor.Product.Default
import qualified Database.PostgreSQL.Simple as PSQL
import Effectful
import Effectful.Dispatch.Dynamic
import qualified Effectful.PostgreSQL.Connection as Conn
import Effectful.TH
import qualified Opaleye as O
import qualified Opaleye.Internal.Inferrable as O

data Opaleye :: Effect where
  RunSelectExplicit :: O.FromFields fields haskells -> O.Select fields -> Opaleye m [haskells]
  RunSelectFoldExplicit ::
    O.FromFields fields haskells ->
    O.Select fields ->
    b ->
    (b -> haskells -> m b) ->
    Opaleye m b
  RunInsert :: O.Insert haskells -> Opaleye m haskells
  RunDelete :: O.Delete haskells -> Opaleye m haskells
  RunUpdate :: O.Update haskells -> Opaleye m haskells

makeEffect ''Opaleye

runSelect ::
  (HasCallStack, Opaleye :> es, Default O.FromFields fields haskells) =>
  O.Select fields ->
  Eff es [haskells]
runSelect = runSelectExplicit def

runSelectFold ::
  (HasCallStack, Opaleye :> es, Default O.FromFields fields haskells) =>
  O.Select fields ->
  b ->
  (b -> haskells -> Eff es b) ->
  Eff es b
runSelectFold = runSelectFoldExplicit def

runSelectI ::
  (HasCallStack, Opaleye :> es, Default (O.Inferrable O.FromFields) fields haskells) =>
  O.Select fields ->
  Eff es [haskells]
runSelectI = runSelectExplicit (O.runInferrable def)

runOpaleyeWithConnection ::
  (HasCallStack, Conn.WithConnection :> es, IOE :> es) =>
  Eff (Opaleye : es) a ->
  Eff es a
runOpaleyeWithConnection = interpret $ \env -> \case
  RunSelectExplicit ff sel -> Conn.withConnection $ \conn -> liftIO $ O.runSelectExplicit ff conn sel
  RunSelectFoldExplicit ff sel initial foldFn ->
    Conn.withConnection $ \conn ->
      localSeqUnliftIO env $ \unlift ->
        liftIO $ O.runSelectFoldExplicit ff conn sel initial (\acc new -> unlift $ foldFn acc new)
  RunInsert sel -> Conn.withConnection $ \conn -> liftIO $ O.runInsert conn sel
  RunDelete sel -> Conn.withConnection $ \conn -> liftIO $ O.runDelete conn sel
  RunUpdate sel -> Conn.withConnection $ \conn -> liftIO $ O.runUpdate conn sel

runOpaleyeConnection ::
  (HasCallStack, IOE :> es) =>
  PSQL.Connection ->
  Eff (Opaleye : es) a ->
  Eff es a
runOpaleyeConnection conn = interpret $ \env -> \case
  RunSelectExplicit ff sel -> liftIO $ O.runSelectExplicit ff conn sel
  RunSelectFoldExplicit ff sel initial foldFn ->
    localSeqUnliftIO env $ \unlift ->
      liftIO $ O.runSelectFoldExplicit ff conn sel initial (\acc new -> unlift $ foldFn acc new)
  RunInsert sel -> liftIO $ O.runInsert conn sel
  RunDelete sel -> liftIO $ O.runDelete conn sel
  RunUpdate sel -> liftIO $ O.runUpdate conn sel

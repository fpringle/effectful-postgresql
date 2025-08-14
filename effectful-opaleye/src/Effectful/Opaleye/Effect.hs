{-# LANGUAGE TemplateHaskell #-}

module Effectful.Opaleye.Effect
  ( -- * Effect
    Opaleye (..)

    -- ** Effectful functions
  , runSelectExplicit
  , runSelectFoldExplicit
  , runInsert
  , runDelete
  , runUpdate
  )
where

import Effectful
import Effectful.TH
import qualified Opaleye as O

-- | A dynamic effect to perform @opaleye@ operations.
data Opaleye :: Effect where
  -- | Lifted 'O.RunSelectExplicit'.
  RunSelectExplicit :: O.FromFields fields haskells -> O.Select fields -> Opaleye m [haskells]
  -- | Lifted 'O.RunSelectFoldExplicit'.
  RunSelectFoldExplicit ::
    O.FromFields fields haskells ->
    O.Select fields ->
    b ->
    (b -> haskells -> m b) ->
    Opaleye m b
  -- | Lifted 'O.RunInsert'.
  RunInsert :: O.Insert haskells -> Opaleye m haskells
  -- | Lifted 'O.RunDelete'.
  RunDelete :: O.Delete haskells -> Opaleye m haskells
  -- | Lifted 'O.RunUpdate'.
  RunUpdate :: O.Update haskells -> Opaleye m haskells

makeEffect ''Opaleye

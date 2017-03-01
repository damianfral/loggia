{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Types where

import           BasicPrelude
import           Control.Lens
import           Data.Ratio
import           GHCJS.Types  as JS
import           Loggia.Types

--------------------------------------------------------------------------------

type Sheet = (Page, Page)

data AppModel = AppModel
  { _pages   :: Vector Sheet
  , _pointer :: Int
  }

makeLenses ''AppModel

--------------------------------------------------------------------------------

data AppSingleAction
  =  MoveLeft
  |  MoveRight
  |  AddImages     [Image]
  |  SplitAt       Int
  |  RemoveImage   Int
  |  RemoveMockup  Int
  |  RemoveSplit   Int
  |  RandomArrange
  |  SetScale      (Ratio Int)

--------------------------------------------------------------------------------

type AppAction = [AppSingleAction]
type AppUpdate = AppAction -> Album -> LoggiaT Album

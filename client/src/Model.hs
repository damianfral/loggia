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
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Model where

import           BasicPrelude
import           Control.Arrow  ((>>>))
import           Control.Lens
import           Control.Monad
import           Data.Monoid
import qualified Data.Semigroup as SG
import           Loggia.Types

--------------------------------------------------------------------------------

data AppModel = AlbumViewerModel
  { _pointer :: Int
  , _pages   :: [Page]
  , _images  :: [Image]
  , _mockups :: [Mockups]
  , _album   :: [([Image], Maybe Mockup)]
  } deriving (Show, Eq)

makeLenses ''AppModel

--------------------------------------------------------------------------------

data AppEvent
  =  MoveLeft
  |  MoveRight
  |  DontMove
  |  AddImages     [Image]
  |  RemoveImages  [Image]
  |  AddSplit      Int
  |  RemoveSplit   Int
  |  AddMockups    [Mockup]
  |  RemoveMockups [Mockup]
  |  RandomArrange Int
  deriving (Show, Eq)

--------------------------------------------------------------------------------

type AppUpdate = AppEvent -> AppModel -> AppModel

applyUpdate :: AppUpdate
applyUpdate update model = model
  & editorModel %~ applyEditorUpdate (update ^. editorUpdate)
  & viewerModel %~ (go >>> applyViewerUpdate (update ^. viewerUpdate))
  where
    go = clampPointer

applyEditorUpdate :: EditorUpdate -> EditorModel -> EditorModel
applyEditorUpdate (AddImages xs) = editorModel

applyViewerUpdate MoveLeft  = pointer -~ 1
applyViewerUpdate MoveRight = pointer +~ 1
applyViewerUpdate DontMove  = id

--------------------------------------------------------------------------------

makePairs :: Int -> [SPage] -> [(Int, (SPage, SPage))]
makePairs _ []       = []
makePairs _ [x]      = []
-- !!!!!!!!!! Wrong!
makePairs i (x:y:xs) = (i,(x,y)):(makePairs (i+1) xs)

unmakePairs []          = []
unmakePairs [(_,(a,b))] = [a,b]
unmakePairs (x:xs)      = unmakePairs [x] <> unmakePairs xs

clamp a b x
   | x < a     = a
   | x > b     = b
   | otherwise = x

clampPointer :: Model -> Model
clampPointer m = m & pointer %~ clamp 0 l
  where l = max 0 $ length (m ^. album) - 1



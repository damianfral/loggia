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
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module App where

import           BasicPrelude
import           Control.Arrow        ((>>>))
import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Random
import           Data.Either
import           Data.FileEmbed
import           Data.Monoid
import           Data.Ratio
import qualified Data.Semigroup       as SG
import qualified Data.Vector          as V
import           Loggia.Core
import           Loggia.Mockup
import           Loggia.Parser
import           Loggia.Types
import           Reflex.Dom
import           Types
import           Widget

--------------------------------------------------------------------------------

albumToAppModel :: Album -> AppModel
albumToAppModel (Album ls c rs _) = AppModel pags p
  where
    pags = V.fromList $ makePairs $ (reverse ls) <> [c] <> rs
    p    = length ls

--------------------------------------------------------------------------------

appUpdate :: AppUpdate
appUpdate action = foldl1 (>=>) $ map go action
  where
    go action album = case action of
      MoveLeft         -> return . turnPageLeft  $ album
      MoveRight        -> return . turnPageRight $ album
      AddImages     xs -> do
        a <- arrange (album ^. albumAspectRatio) =<< randomSplits xs
        return $ a
      _ -> return album
  -- AddMockups    xs -> over mockups $ flip (<>) xs
  -- SplitAt       i  -> over album   $ searchAndSplit i
  -- RemoveImage   i  -> id
  -- RemoveMockup  i  -> id
  -- RemoveSplit   i  -> id
  -- RandomArrange i  -> id
  -- where
  --   searchAndSplit i ((ys,m):xs)
  --     | i < length ys = ((ys',Nothing):(ys'',Nothing):xs)
  --     | otherwise     = (ys,m):(searchAndSplit (i - (length ys)) xs)
  --     where (ys',ys'') = splitAt i ys

--------------------------------------------------------------------------------

randomSplits :: [a] -> LoggiaT [[a]]
randomSplits xs = do
  i <- getRandomR (1,5)
  case length xs <= i of
    True  -> return [ xs ]
    False -> do
      let (a,b) = splitAt (min i (length xs)) xs
      mappend <$> (pure [a]) <*> (randomSplits b)

makePairs :: [Page] -> [Sheet]
makePairs []       = []
makePairs [x]      = [(x, EmptyPage)]
makePairs (x:y:xs) = (x,y):(makePairs xs)

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery i xs
  | i <= 0    = repeat []
  | otherwise = xs1:(splitEvery i xs2)
  where (xs1,xs2) = splitAt (min i $ length xs) xs

splitEverys :: [Int] -> [a] -> [[a]]
splitEverys []     xs = [xs]
splitEverys _      [] = []
splitEverys (i:is) xs = do
  ys <- splitEvery i xs
  splitEverys is ys

layouts :: IO [Layout]
layouts = do
  let files = $(embedDir "./data/svg/")
  let l = over _2 (mockupToLayouts <=< parseMockup) <$> files
  -- mapM_ go l
  return $ mconcat $ catMaybes $ (view _2 <$> l)
    -- here
    --   go (a, Just m) = putStrLn (show a) >> putStrLn (show $ countLayoutLeaves m)
    --   go (a, _) = putStrLn (show a)

app :: MonadWidget t m => m ()
app = do
  ls <- liftIO layouts
  seed   <- liftIO getStdGen
  liftIO $ print $ length ls
  loadEv <- imageLoader
  rec albumDyn <- foldDyn (go ls seed) initialAlbum (loadEv <> actionEv)
      appModel <- mapDyn albumToAppModel albumDyn
      actionEv <- renderAlbum appModel
  return ()
  where
    initialAlbum = Album [] EmptyPage [] (16 % 9)
    go :: [Layout] -> StdGen -> AppAction -> Album -> Album
    go ls seed action album = either (const initialAlbum) id $ runLoggiaT seed ls $ appUpdate action album


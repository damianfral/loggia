{-# LANGUAGE FlexibleContexts         #-}

module Loggia.Utils where

import           BasicPrelude
import           Control.Lens
import System.Random

--------------------------------------------------------------------------------

splits :: [a] -> [([a],[a])]
splits []  = []
splits [_] = []
splits xs  = flip splitAt xs <$> [1 .. (length xs) - 1]

minimumOn :: (Ord b) => (a -> b) -> [a] -> Maybe a
minimumOn _ [] = Nothing
minimumOn f xs = pure $ view _1 $ minimumBy (\x y ->
    (compare (x ^. _2) (y ^. _2))) $ zip xs (f <$> xs)

maximumOn :: (Ord b) => (a -> b) -> [a] -> Maybe a
maximumOn _ [] = Nothing
maximumOn f xs = pure $ view _1 $ maximumBy (\x y ->
    (compare (x ^. _2) (y ^. _2))) $ zip xs (f <$> xs)

shuffle :: [a] -> IO [a]
shuffle [] = return []
shuffle xs = do
    randomPosition <- getStdRandom (randomR (0, length xs - 1))
    let (left, (a:right)) = splitAt randomPosition xs
    (a:) <$> (shuffle (left ++ right))

between :: Ord a => a -> a -> a -> Bool
between a b x = a <= x && x <= b

flattenlistOfTuples :: [(t, t)] -> [t]
flattenlistOfTuples [] = []
flattenlistOfTuples ((a,b):xs) = [a,b] <> flattenlistOfTuples xs

atMiddle :: [a] -> Maybe a
atMiddle [] = Nothing
atMiddle xs = Just $ xs !! (length xs `div` 2)

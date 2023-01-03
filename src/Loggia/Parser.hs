{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Loggia.Parser where

import BasicPrelude
import Control.Arrow ((>>>))
import Data.Ratio
import Data.Text (pack)
import qualified Graphics.Svg as SVG
import Loggia.Types
import System.Directory
import Turtle.Prelude

--------------------------------------------------------------------------------

numberToRatio :: SVG.Number -> Ratio Integer
numberToRatio (SVG.Num x) = toRational x
numberToRatio (SVG.Px x) = toRational x
numberToRatio (SVG.Em x) = toRational x
numberToRatio (SVG.Percent x) = toRational x
numberToRatio (SVG.Pc x) = toRational x
numberToRatio (SVG.Mm x) = toRational x
numberToRatio (SVG.Cm x) = toRational x
numberToRatio (SVG.Point x) = toRational x
numberToRatio (SVG.Inches x) = toRational x

ratioIntegerToRatioInt :: Ratio Integer -> Ratio Int
ratioIntegerToRatioInt = fromRational

numberToRatioInt :: SVG.Number -> Ratio Int
numberToRatioInt = ratioIntegerToRatioInt . numberToRatio

numberToInt :: SVG.Number -> Int
numberToInt (SVG.Num x) = round x
numberToInt (SVG.Px x) = round x
numberToInt (SVG.Em x) = round x
numberToInt (SVG.Percent x) = round x
numberToInt (SVG.Pc x) = round x
numberToInt (SVG.Mm x) = round x
numberToInt (SVG.Cm x) = round x
numberToInt (SVG.Point x) = round x
numberToInt (SVG.Inches x) = round x

loadMockup :: FilePath -> IO (Maybe Mockup1)
loadMockup = SVG.loadSvgFile >=> pure . (>>= parseMockup)

parseMockup :: SVG.Document -> Maybe Mockup1
parseMockup doc = normalizeMockup <$> (Mockup bxs <$> w <*> h)
  where
    bxs = svgToBoxes doc
    vb = go <$> SVG._viewBox doc
    hSegment = fst <$> vb
    vSegment = snd <$> vb
    hSegmentSize = fromRational . segmentSize <$> hSegment
    vSegmentSize = fromRational . segmentSize <$> vSegment
    w = numberToInt <$> SVG._width doc <|> hSegmentSize
    h = numberToInt <$> (SVG._height doc <|> vSegmentSize)
    go :: (Double, Double, Double, Double) -> (Segment, Segment)
    go (h1, v1, h2, v2) =
      ( Segment (round h1) (round h2),
        Segment (round v1) (round v2)
      )

extractRectangles :: [SVG.Tree] -> [SVG.Rectangle]
extractRectangles [] = []
extractRectangles (t : ts) = case t of
  SVG.GroupTree (SVG.Group _ trees _ _) -> extractRectangles (trees <> ts)
  SVG.RectangleTree r -> r : extractRectangles ts
  _ -> extractRectangles ts

isRectangle :: SVG.Tree -> Bool
isRectangle (SVG.RectangleTree _) = True
isRectangle _ = False

svgRectangleToBox :: SVG.Rectangle -> Box
svgRectangleToBox (SVG.Rectangle _ (x, y) w h _) = Box s1 s2
  where
    x' :: Ratio Int = numberToRatioInt x
    y' = numberToRatioInt y
    w' = numberToRatioInt w
    h' = numberToRatioInt h
    s1 = Segment x' (x' + w')
    s2 = Segment y' (y' + h')

svgToBoxes :: SVG.Document -> [Box]
svgToBoxes = SVG._elements >>> extractRectangles >>> fmap svgRectangleToBox

--------------------------------------------------------------------------------

wrapInNixShell :: Text -> Text
wrapInNixShell x = pack $ "nix-shell -p pkgs.graphicsmagick --command " ++ show x

readDimensions :: FilePath -> IO (Maybe (Integer, Integer))
readDimensions file = doesFileExist file >>= go
  where
    go False = return Nothing
    go True = do
      -- Check outpur code to see if the command has been executed properly.
      (_, o) <-
        shellStrict
          ( wrapInNixShell $
              "gm identify -format \'(%w,%h)\' " <> pack file
          )
          ""
      return $ Just (read o :: (Integer, Integer))

loadImage :: FilePath -> IO (Maybe Image)
loadImage path = do
  dimensions <- readDimensions path
  let w = fromIntegral . fst <$> dimensions
  let h = fromIntegral . snd <$> dimensions
  let image = ImageF (pack path) <$> w <*> h <*> pure Nothing
  return $ image

module Loggia.Parser where

import BasicPrelude
import Control.Arrow ((>>>))
import Data.Ratio
import qualified Graphics.Svg as SVG
import Loggia.Types

--------------------------------------------------------------------------------

numberToRatio :: SVG.Number -> Ratio Int
numberToRatio (SVG.Num x) = round (1000 * x) % 1000
numberToRatio (SVG.Px x) = round (1000 * x) % 1000
numberToRatio (SVG.Em x) = round (1000 * x) % 1000
numberToRatio (SVG.Percent x) = round (1000 * x) % 1000
numberToRatio (SVG.Pc x) = round (1000 * x) % 1000
numberToRatio (SVG.Mm x) = round (1000 * x) % 1000
numberToRatio (SVG.Cm x) = round (1000 * x) % 1000
numberToRatio (SVG.Point x) = round (1000 * x) % 1000
numberToRatio (SVG.Inches x) = round (1000 * x) % 1000

loadMockup :: FilePath -> IO (Maybe Mockup1)
loadMockup = SVG.loadSvgFile >=> pure . (>>= svgToMockup)

parseMockup :: ByteString -> Maybe Mockup1
parseMockup = SVG.parseSvgFile "" >=> svgToMockup

svgToMockup :: SVG.Document -> Maybe Mockup1
svgToMockup doc = normalizeMockup <$> (Mockup <$> bxs <*> f w <*> f h)
  where
    w = numberToRatio <$> SVG._width doc
    h = numberToRatio <$> SVG._height doc
    f x = (*) <$> (numerator <$> x) <*> commonDen
    commonDen = (*) <$> (denominator <$> w) <*> (denominator <$> h)
    bxs =
      commonDen >>= \c ->
        return $ fmap (* (c % 1)) <$> svgToBoxes doc

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
    x' = numberToRatio x
    y' = numberToRatio y
    w' = numberToRatio w
    h' = numberToRatio h
    s1 = Segment x' (x' + w')
    s2 = Segment y' (y' + h')

svgToBoxes :: SVG.Document -> [Box]
svgToBoxes = SVG._elements >>> extractRectangles >>> fmap svgRectangleToBox

--------------------------------------------------------------------------------

-- wrapInNixShell :: Text -> Text
-- wrapInNixShell x = "nix-shell -p pkgs.graphicsmagick --command " ++ show x

-- readDimensions :: FilePath -> IO (Maybe (Integer,Integer))
-- readDimensions file =  doesFileExist file >>= go where
--   go False = return Nothing
--   go True  = do
--     -- Check outpur code to see if the command has been executed properly.
--     (_, o) <- shellStrict (wrapInNixShell
--       $ "gm identify -format \'(%w,%h)\' " <> pack file) ""
--     return $ Just (read o::(Integer,Integer))

-- loadImage :: FilePath -> IO (Maybe Image)
-- loadImage path = do
--       dimensions <- readDimensions path
--       let aspectRatio = uncurry (%) <$> dimensions
--       let image = ImageF (pack path) <$> aspectRatio <*> (pure Nothing)
--       return $ image

module Loggia.IO.Parser where

import           BasicPrelude
import           Control.Arrow      ((>>>))
import           Data.Ratio
import           Data.Text          (pack)
import qualified Graphics.Svg       as SVG
import qualified Graphics.Svg.Types as SVG
import           Loggia.Segment
import           Loggia.Types
import           System.Directory
import           Turtle.Prelude
import           Turtle.Shell       hiding (view)

--------------------------------------------------------------------------------

numberToRatio :: SVG.Number -> Ratio Integer

numberToRatio (SVG.Num     x) = toRational x
numberToRatio (SVG.Px      x) = toRational x
numberToRatio (SVG.Em      x) = toRational x
numberToRatio (SVG.Percent x) = toRational x
numberToRatio (SVG.Pc      x) = toRational x
numberToRatio (SVG.Mm      x) = toRational x
numberToRatio (SVG.Cm      x) = toRational x
numberToRatio (SVG.Point   x) = toRational x
numberToRatio (SVG.Inches  x) = toRational x

loadMockup :: FilePath -> IO (Maybe Mockup1)
loadMockup = SVG.loadSvgFile >=> pure . (>>= parseMockup)

parseMockup :: SVG.Document -> Maybe Mockup1
parseMockup doc = normalizeMockup <$> (Mockup bxs <$> w <*> h)
  where
    bxs = svgToBoxes doc
    vb    = go <$> SVG._viewBox doc
    w     = numberToRatio <$> SVG._width  doc <|> (segmentSize . fst <$> vb)
    h     = numberToRatio <$> SVG._height doc <|> (segmentSize . snd <$> vb)
    go :: (Double, Double, Double, Double) -> (Segment, Segment)
    go (h1, v1, h2, v2) = ( Segment (round h1 % 1) (round h2 % 1)
                          , Segment (round v1 % 1) (round v2 % 1))

extractRectangles :: [SVG.Tree] -> [SVG.Rectangle]
extractRectangles [] = []
extractRectangles (t:ts) = case t of
  SVG.GroupTree  (SVG.Group  _ trees _) -> extractRectangles (trees <> ts)
  SVG.RectangleTree r -> r:(extractRectangles ts)
  _ -> extractRectangles ts

isRectangle :: SVG.Tree -> Bool
isRectangle (SVG.RectangleTree _) = True
isRectangle _ = False

svgRectangleToBox :: SVG.Rectangle -> Box
svgRectangleToBox (SVG.Rectangle _ (x,y) w h _) = Box s1 s2
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

wrapInNixShell :: Text -> Text
wrapInNixShell x = "nix-shell -p pkgs.graphicsmagick --command " ++ show x

readDimensions :: FilePath -> IO (Maybe (Integer,Integer))
readDimensions file =  doesFileExist file >>= go where
  go False = return Nothing
  go True  = do
    -- Check outpur code to see if the command has been executed properly.
    (_, o) <- shellStrict (wrapInNixShell
      $ "gm identify -format \'(%w,%h)\' " <> pack file) ""
    return $ Just (read o::(Integer,Integer))

loadImage :: FilePath -> IO (Maybe Image)
loadImage path = do
      dimensions <- readDimensions path
      let aspectRatio = uncurry (%) <$> dimensions
      let image = ImageF (pack path) <$> aspectRatio <*> (pure Nothing)
      return $ image

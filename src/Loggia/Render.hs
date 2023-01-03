{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns     #-}

module Loggia.IO.Render where

import           BasicPrelude
import           Control.Arrow          ((>>>))
import           Control.Lens           hiding ((:<))
import           Control.Monad.State
import           Data.Generics.Fixplate
import           Data.Ratio
import           Loggia.Box
import           Loggia.Types
import           Lucid.Base
import           Lucid.Html5

(!) :: (With a) => a -> [Attribute] -> a
(!) = with

toCalc, toCalc', toCalcPx :: (Integral r, Show r) => Ratio r -> Text
toCalc e  = unwords [ "calc("
                    , show (100 * numerator e) <> "% /"
                    , show $ denominator e
                    , ")" ]

toCalc' e  = unwords [ "calc("
                    , show (numerator e) <> " /"
                    , show $ denominator e
                    , ")" ]

toCalcPx e  = unwords [ "calc("
                      , show (numerator e) <> "px /"
                      , show $ denominator e
                      , ")" ]

attrsToCSS :: Attrs -> Text
attrsToCSS attrs = intercalate ";" $
  [ if attrs ^. direction == Just Vertical then "flex-direction:column" else ""
  , "flex-grow:"     <> toCalc'  (attrs ^. relSize)
  , "margin-top:"    <> toCalcPx (512   *  (attrs ^. margins . top))
  , "margin-bottom:" <> toCalcPx (512   *  (attrs ^. margins . bottom))
  , "margin-left:"   <> toCalcPx (512   *  (attrs ^. margins . left))
  , "margin-right:"  <> toCalcPx (512   *  (attrs ^. margins . right))
  ]

imageToCSS :: Image  -> Text
imageToCSS (ImageF p _ _) = ";background-image: url(" <> p <> ");"

renderPageHtml :: Page -> Html ()
renderPageHtml = generateDivs >>> flip evalState 0
  where
    generateDivs :: Page -> State Int (Html ())
    generateDivs (unFix -> x :< Leaf a) = do
        i <- modify (+1) >> get
        return $ divLeave_ x a  (toHtml $ "Box " <> show i)
    generateDivs (unFix -> x :< Branch f g) = divNode_ x <$>
      (mappend <$> generateDivs f <*> generateDivs g)
    divNode_  attrs     = div_ [ style_ $ attrsToCSS attrs
                               , class_ "photo-group" ]
    divLeave_ attrs img = div_ [ style_ (attrsToCSS attrs <> imageToCSS img )
                               , class_ "photo-item"  ]

defaultTemplate :: [Html ()] -> Html ()
defaultTemplate albumPages = html_ $ do
  head_ $ do
    style_ "div {display: flex; flex: 1 1 auto; background-size: cover; background-position: 50% 50%;} html, body {height: 100%; width: 100%; margin: 0; padding: 0;} .album-page {height: 100%; width: 100%;}"
    link_ [type_ "text/css", rel_ "stylesheet", href_ "style.css"]
  body_ $ do
     div_ `with` [class_ "album"] $ mapM_ go (zip [1..] $ makePairs albumPages)
     script_ [src_ "../loggia-client/dist/build/loggia-client/loggia-client.jsexe/all.js", type_ "text/javascript"] $ (mempty::Text)

  where
    -- makePairs :: [a] -> [(a,a)]
    makePairs [] = []
    makePairs [x] = [(x,div_ "")]
    makePairs (x:y:xs) = (x,y):(makePairs xs)
    go :: (Int, (Html (), Html ())) -> Html ()
    go (i,(l,r)) = div_ `with` [ id_ ("sheet" <> show i), class_ "album__sheet"] $ do
                      div_ `with` [class_ "album__page album__page--left" ] $ l
                      div_ `with` [class_ "album__page album__page--right" ] $ r

generateAlbum :: [Page] -> Html ()
generateAlbum = fmap renderPageHtml >>> defaultTemplate

writeAlbum :: FilePath -> [Page] -> IO ()
writeAlbum file = generateAlbum >>> renderToFile file

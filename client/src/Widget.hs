{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE JavaScriptFFI         #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Widget where
import           BasicPrelude                     as P
import           Control.Arrow                    ((>>>))
import           Control.Lens
import           Control.Monad.Ref
import           Data.Dependent.Sum
import           Data.Generics.Fixplate
import           Data.Generics.Fixplate.Morphisms
import qualified Data.JSString                    as JSS
import           Data.JSString.Text               (textFromJSVal)
import           Data.Map
import           Data.Maybe
import           Data.Ratio
import           Data.Text                        (pack)
import qualified Data.Vector                      as V
import           GHCJS.DOM
import           GHCJS.DOM.Blob
import           GHCJS.DOM.Document               (createElement)
import           GHCJS.DOM.Element
import           GHCJS.DOM.EventM                 as EvM
import           GHCJS.DOM.File
import qualified GHCJS.DOM.FileReader             as FR
import           GHCJS.DOM.HTMLImageElement       (castToHTMLImageElement,
                                                   getHeight, getWidth, setSrc)

import qualified GHCJS.DOM.HTMLImageElement       as IMG
import           GHCJS.DOM.Types                  hiding (Event, Text)
import           GHCJS.Foreign
import           GHCJS.Marshal
import           GHCJS.Types                      as JS
import qualified Loggia.Types                     as L
import           Reflex
import           Reflex.Dom                       as RDom hiding (getKeyEvent)
import           Reflex.Dom.Class
import           Reflex.Dom.Widget.Input
import           Reflex.Host.Class
import           Types

import qualified Data.List.NonEmpty               as NE

--------------------------------------------------------------------------------

toCalc, toCalcNoUnit, toCalcPx :: (Integral r, Show r) => Ratio r -> Text
toCalc e  = unwords [ "calc("
                    , show (100 * numerator e) <> "% /"
                    , show $ denominator e
                    , ")" ]

toCalcNoUnit e = unwords [ "calc("
                         , show (numerator e) <> " /"
                         , show $ denominator e
                         , ")" ]

toCalcPx e  = unwords [ "calc("
                      , show (numerator e) <> "px /"
                      , show $ denominator e
                      , ")" ]

attrsToCSS :: L.Attrs -> Text
attrsToCSS attrs = intercalate ";" $
  [ if attrs ^. L.direction == Just L.Vertical then "flex-direction:column" else ""
  , "flex-grow:"     <> toCalcNoUnit  (attrs ^. L.relSize)
  , "margin-top:"    <> toCalcPx (512   *  (attrs ^. L.margins . L.top))
  , "margin-bottom:" <> toCalcPx (512   *  (attrs ^. L.margins . L.bottom))
  , "margin-left:"   <> toCalcPx (512   *  (attrs ^. L.margins . L.left))
  , "margin-right:"  <> toCalcPx (512   *  (attrs ^. L.margins . L.right))
  ]

imageToCSS :: L.Image  -> Text
imageToCSS (L.ImageF p _ _ _) = ";background-image: url(" <> p <> ");"

--------------------------------------------------------------------------------

-- renderPageNode :: (MonadWidget t m) => L.Page -> m ()
-- renderPageNode :: (MonadWidget t m) => L.TaggedTreeF L.Attrs L.Image (L.TaggedTreeF m ())
renderPageNode :: MonadWidget t m => L.TaggedTreeF L.Attrs L.Image (m ()) -> m ()
renderPageNode (att L.:< L.Leaf img) = do
  el <- elAttr "div" domAttrs $ text ""
  return ()
  where
    domAttrs = mconcat
      [ "class" =: "photo-item"
      , "style" =: ( textToString $  attrsToCSS att <> imageToCSS img) ]

renderPageNode (att L.:< L.Branch l r) = elAttr "div" domAttrs  $ l >> r
  where
    domAttrs = mconcat
      [ "class" =: "photo-group"
      , "style" =: ( textToString $ attrsToCSS att ) ]

renderPage :: (MonadWidget t m) => AppAction -> L.Page -> m (Event t AppAction)
renderPage _ L.EmptyPage = elAttr' "div" ("class" =: "album__page") (text "")  >> return never
renderPage action (L.Page tree) = do
  (el, _) <- elAttr' "div" ("class" =: "album__page") $
    cata renderPageNode tree
  return $ tag (pure action) $ domEvent Click el

renderSheet :: (MonadWidget t m)
            => Dynamic t Int -> Int -> (L.Page, L.Page) -> m (Event t AppAction)
renderSheet pointerDyn i (l,r) = do
  attrs <- mapDyn go pointerDyn
  elDynAttr "div" attrs $ do
    lEv <- renderPage [ MoveLeft  ] l
    rEv <- renderPage [ MoveRight ] r
    return $ lEv <> rEv
  where
    go pointer = mconcat
      [ "data-pagenumber" =: (textToString $ show i)
      , "class" =: ( "album__sheet "
            <> case pointer `compare` i of
                 LT -> "album__sheet--folded right-folded "
                 EQ -> "sheet album__sheet--unfolded "
                 GT -> "album__sheet--folded left-folded "
            <> case abs (pointer - i) > 2 of
                 True -> "hidden"
                 _    -> "" )

      ]

renderSheets :: (MonadWidget t m) => Dynamic t Int
             -> Vector (L.Page, L.Page) -> m (Event t AppAction)
renderSheets point sheets = do
  evs <-  sequence $ V.reverse $ V.imap (renderSheet point) $ sheets
  return $ V.toList >>> mconcat $ evs

-- The first parameter is used for performance reaseon. Need to study how to
-- use the correspongind field in model (second parameter) avoiding non-changes
-- updates in that  field.
renderAlbum :: (MonadWidget t m)
            => Dynamic t AppModel -> m (Event t AppAction)
renderAlbum modelDyn = do
  pagesDyn    <- nubDyn <$> mapDyn (view pages)   modelDyn
  pointerDyn' <- nubDyn <$> mapDyn (view pointer) modelDyn
  pointerDyn  <- combineDyn clamping pagesDyn pointerDyn'
  pageElsDyn  <- forDyn pagesDyn $ divClass "album" . renderSheets pointerDyn
  updateEvEv  <- dyn pageElsDyn
  updateEvB   <- hold never updateEvEv
  controlsEv  <- controlsWidget
  let updateEv = switch updateEvB
  return $ updateEv <> controlsEv
    where clamping v = clamp 0 (length v - 1)

-- albumViewer :: forall t m. RDom.MonadWidget t m => Text ->  m ()
-- albumViewer albumName = do
--   let getAlbumByID :<|> _ = client (Proxy :: Proxy PI)
--                             (Proxy  :: Proxy m)
--                             (constDyn $ BaseUrl Http "" 8080 "")
--   -- Needed to fire the "start" event.
--   -- bE <- button "view"
--   -- -- May change (servant-reflex)
--   -- albumLoadEv  <- (fmap (makePairs 0 . (fromMaybe []) . fst)) <$> getAlbumByID (constant (Just albumName)) bE
--   albumLoadEv <- editorWidget
--   albumDyn     <- holdDyn [] albumLoadEv

--   -- Keyboard events.
--   -- eKeydown <- wrapDomEvent e elementOnkeydown getKeyEvent
--   -- let leftKeyEvent  = tag (constant MoveLeft ) $ ffilter ((==) 37 . keKeyCode) eKeypress
--   -- let rightKeyEvent = tag (constant MoveRight) $ ffilter ((==) 38 . keKeyCode) eKeypress

--   rec model    <- foldDyn applyUpdate initialModel ((LoadAlbum <$> albumLoadEv) <> updateEv )
--       updateEv <- renderAlbum albumDyn model

--   return ()
--   where initialModel = AppModel 0 []

controlsWidget :: MonadWidget t m => m (Event t AppAction)
controlsWidget = divClass "controls" $ do
  l <- button "◀"
  r <- button "▶"
  return $ (const [ MoveLeft ] <$> l) <> (const [ MoveRight ] <$> r)

clamp a b = (max a) . (min b)

--------------------------------------------------------------------------------

-- instance FromHttpApiData [L.Page] where
--   parseUrlPiece x = Right $ read x

--------------------------------------------------------------------------------

imageLoader :: (MonadSample t m, MonadWidget t m) => m (Event t AppAction)
imageLoader = do
  dynFiles  <- fileInputWidget
  dynFilesL <- mapDyn length dynFiles
  display dynFilesL
  imagesEvDyn <- mapDyn readImages dynFiles
  imagesEv    <- dynSwitch imagesEvDyn
  return $  fmapMaybe ( Just . (:[]) . AddImages ) imagesEv

dynSwitch :: MonadWidget t m => Dynamic t (m (Event t a)) -> m (Event t a)
dynSwitch = dyn >=> switchPromptly never

fileInputWidget :: MonadWidget t m => m (Dynamic t [File])
fileInputWidget = value <$> fileInput config
  where
    config = FileInputConfig $ constDyn $ fromList
      [ ("type", "file")
      , ("multiple", "true")
      ]

getResultString :: FileReader -> IO Text
getResultString fr = do
  v <- FR.getResult fr
  s <- fromJSVal v
  return $ pack $ fromMaybe "" (fromJSString <$> s)

readDimensions :: MonadWidget t m => L.Image -> m (Event t L.Image)
readDimensions (L.ImageF p _ _ x) = do
  Just doc  <- liftIO $ currentDocument
  Just el   <- liftIO $ createElement doc $ Just ("img"::String)
  let img   =  castToHTMLImageElement $ el
  let handler :: (L.Image -> IO ()) -> EventM HTMLImageElement UIEvent ()
      handler k = liftIO $ k =<< do
        w    <- getWidth img
        h    <- getHeight img
        print (w,h)
        return $ L.ImageF p w h x
  eLoad <- buildEvent (EvM.on img load . handler)
  setSrc img p
  return eLoad

-- readImage :: MonadWidget t m => File -> m (Event t L.Image)
-- readImage file = do
--   fileReader <- FR.newFileReader
--   let handler :: (Text -> IO ()) -> EventM FileReader UIEvent ()
--       handler k = liftIO $ getResultString fileReader >>= k
--   eLoad <- buildEvent (EvM.on fileReader  FR.load . handler)
--   liftIO $ FR.readAsDataURL fileReader ( Just $ toBlob file )
--   (holdDyn (return never) (fmap go eLoad) >>= dynSwitch)
--     where
--       go :: MonadWidget t m => Text -> m (Event t L.Image)
--       go x  = readDimensions $ L.ImageF x 1 1 Nothing

readImage :: MonadWidget t m => File -> m (Event t L.Image)
readImage file = do
  fileReader <- FR.newFileReader
  Just doc  <- liftIO $ currentDocument
  Just el   <- liftIO $ createElement doc $ Just ("img"::String)
  let img   =  castToHTMLImageElement $ el
  liftIO $ setSrc img (""::String)
  imgLoadEv <- wrapDomEvent img (`EvM.on` load) $ liftIO $ do
            putStrLn "Done!"
            p <- getResultString fileReader
            w <- getWidth  img
            h <- getHeight img
            print (w,h)
            return $ L.ImageF p w h Nothing

  let f =
        do
          liftIO $ getResultString fileReader >>= setSrc img
          return imgLoadEv

  evB <- wrapDomEvent fileReader (`EvM.on` FR.load) $ f

  liftIO $ FR.readAsDataURL fileReader ( Just $ toBlob file )
  ev  <- holdDyn never evB
  return $ switchPromptlyDyn ev

readImages :: MonadWidget t m => [File] -> m (Event t [L.Image])
readImages imgs = do
  evList <- mapM readImage imgs
  allEvs <- foldDyn (<>) [] $ (NE.toList <$> mergeList evList)
  return $ updated allEvs  -- ffilter (sameLength imgs) $ updated allEvs
    where
      sameLength :: [a] -> [b] -> Bool
      sameLength a b = length a == length b

askPostEvent :: MonadWidget t m => m (EventTrigger t a -> a -> IO ())
askPostEvent = do
  postGui <- RDom.askPostGui
  runWithActions <- askRunWithActions
  return (\t a -> postGui $ runWithActions [t :=> Identity a])


-- -- askPostEvent :: MonadWidget t m => m (EventTrigger t a -> a -> IO ())
-- -- askPostEvent = do
-- --   postGui <- askPostGui
-- --   runWithActions <- askRunWithActions
-- --   return (\t a -> postGui $ runWithActions [t :=> a])


buildEvent :: (MonadWidget t m)
           => ((a -> IO ()) -> IO (IO ()))
           -> m (Event t a)
buildEvent install = do
  postEvent <- askPostEvent
  newEventWithTrigger (install . postEvent)

-- dataURLFileReader :: (MonadWidget t m) => File -> m (Event t Text)
-- dataURLFileReader file = do
--   fileReader <- liftIO newFileReader
--   let getResultString :: FileReader -> IO (Maybe String)
--       getResultString fr = do
--         v <- getResult fr
--         s <- fromJSVal v
--         return (fmap fromJSString s)
--       handler :: (Maybe String -> IO ()) -> EventM FileReader UIEvent ()
--       handler k = liftIO $ k =<< getResultString fileReader
--   readAsDataURL fileReader (Just file)
--   e <- buildEvent (on fileReader load . handler)
--   return (fmapMaybe id e)

--   fileContent <- getResult reader
--   bs <- liftIO $ fromJSVal fileContent
--   return $ ImageF "" 1 bs

-- editorWidget :: MonadWidget t m => m (Event t AppUpdate)
-- editorWidget = do
--   dynMockups <- divClass "mockups" $ fileInputWidget
--   dynImages  <- divClass "images"  $ fileInputWidget

--   ev <- dyn $ mapDyn (mapM readImage) dynFiles
--   return $ (LoadAlbum . makePairs 0) <$> ev

-- navigationWidget :: MonadWidget t m => Dynamic t AppModel -> (Event t
-- AppUpdate)
-- navigationWidget = divClass "navigation" $ do




--------------------------------------------------------------------------------




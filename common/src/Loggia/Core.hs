{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ViewPatterns          #-}

module Loggia.Core
  ( arrange
  , leftSplitPageAt
  , rightSplitPageAt
  , leftJoinPages
  , rightJoinPages
  , mockupToLayout
  , mockupToLayouts
  ) where

import           BasicPrelude              hiding ((<>))
import           Control.Arrow             ((>>>))
import           Control.Lens              hiding ((:<))
import           Control.Monad.Except
import           Control.Monad.Random
import           Control.Monad.Reader
import           Control.Monad.State       hiding (fix)
import           Control.Monad.Trans.Maybe
import           Data.Generics.Fixplate    hiding (sum)
import           Data.Ratio
import           Data.Semigroup
import           Loggia.Box
import           Loggia.Mockup
import           Loggia.Segment
import           Loggia.Types
import           Loggia.Utils

--------------------------------------------------------------------------------

-- | Transform a @BinaryTree Box@ into a @Layout@ setting its tag to a given
-- value.

-- attachTag :: BinaryTree Box -> Layout
attachTag :: tag -> BinaryTree a -> TaggedTree tag a
attachTag t = cata $ (:<) t >>> Fix

attachEmptyTag :: BinaryTree Box -> Layout
attachEmptyTag = attachTag emptyAttrs

-- | Tag a node with the minimum bounding box that contains its children.
tagWithDimension :: Tagger
tagWithDimension (attrs :< Leaf a) = Fix $ (attrs & boundingBox .~ a) :< Leaf a
tagWithDimension (attrs :< (Branch b1 b2)) = Fix $ attrs'  :< Branch b1 b2
  where
    bb     = (unFix b1 ^. tag . boundingBox) <> (unFix b2 ^. tag . boundingBox)
    attrs' = attrs & boundingBox .~ bb

-- | Tag a node with the relative direction between its 2 branches. If it's a
-- @Leaf@, set @Nothing@.
tagWithDirection :: Tagger
tagWithDirection (a :< Leaf b) = Fix $ (a & direction .~ Nothing) :< Leaf b
tagWithDirection (t :< b@(Branch b1 b2)) = Fix $ (:< b)
  ( t & direction .~ ( Just $ getDirection
      (unFix b1 ^. tag . boundingBox)
      (unFix b2 ^. tag . boundingBox)))

-- | Bottom up traverse a @Layout@ applying @tagWithDimension@ and @tagWithDirection@.
bottomUpTagWithGeometry :: Layout -> Layout
bottomUpTagWithGeometry =  cata $ tagWithDimension >>> unFix >>> tagWithDirection

--------------------------------------------------------------------------------

isVerticalNode, isHorizontalNode :: TaggedTree Attrs a -> Bool
isVerticalNode   = unFix >>> view (tag . direction) >>> (== Just Vertical)
isHorizontalNode = unFix >>> view (tag . direction) >>> (== Just Horizontal)

-- | Tag the second child of a branch with @relSize = 1@ and the first child with
-- the relative size to the second one. The relative size makes references to
-- width in horizontal nodes and height in vertical ones.
tagWihRelativeSizes :: Tagger
tagWihRelativeSizes n@(_     :< Leaf _) = Fix n
tagWihRelativeSizes n@(attrs :< Branch (Fix b1) (Fix b2) )
  | isHorizontalNode $ Fix n = Fix $ attrs :< uncurry Branch
    (go (segmentSize . _horizontal))
  | otherwise                = Fix $ attrs :< uncurry Branch
    (go (segmentSize . _vertical))
  where
    rel f = f (b1 ^. tag . boundingBox) / f (b2 ^. tag . boundingBox)
    go  f = ( Fix $ b1 & tag . relSize  .~  rel f
            , Fix $ b2 & tag . relSize  .~  1)

-- | Tag node's children with margins between them trying to minimize the
-- variance.
tagWithMargins :: Tagger
tagWithMargins (attrs :< Branch b1 b2)
  | isHorizontalNode $ Fix n = Fix $ attrs :< Branch
    (go ghostBox1 b1) (go ghostBox2 b2)
  | isVerticalNode $ Fix n = Fix $ attrs :< Branch
    (go ghostBox3 b1) (go ghostBox4 b2)
  where
    n = attrs :< Branch b1 b2
    middleVLine =
      ( (unFix b2 ^. tag . boundingBox . left)
      + (unFix b1 ^. tag . boundingBox . right) ) / 2
    middleHLine =
      ( (unFix b2 ^. tag . boundingBox . top     )
      + (unFix b1 ^. tag . boundingBox . bottom) ) / 2
    ghostBox1 = attrs ^. boundingBox & right  .~ middleVLine
    ghostBox2 = attrs ^. boundingBox & left   .~ middleVLine
    ghostBox3 = attrs ^. boundingBox & bottom .~ middleHLine
    ghostBox4 = attrs ^. boundingBox & top    .~ middleHLine
    go :: Box -> Layout -> Layout
    go ghostBox b = Fix $ unFix b & tag . margins .~ offsets where
      offsets = calculateOffsets ghostBox (unFix b ^. tag . boundingBox)
tagWithMargins x = Fix x

tagWithMarginsRoot :: Layout -> Layout
tagWithMarginsRoot (unFix -> n) = Fix $ n & tag . margins .~ offsets
  where
    ghostBox = Box unitSegment unitSegment
    offsets  = calculateOffsets ghostBox (n ^. tag . boundingBox)

-- | Tag with offsets and margins between nodes.
bottomUpTagWithRelations :: Layout -> Layout
bottomUpTagWithRelations  = cata tagWihRelativeSizes >>> tagWithMarginsRoot >>> cata tagWithMargins

mockupToLayout :: Mockup1 -> Maybe Layout
mockupToLayout = unflatten >=> (attachEmptyTag >>> bottomUpTagWithGeometry >>> bottomUpTagWithRelations >>> pure)

mockupToLayouts :: Mockup1 -> Maybe [Layout]
mockupToLayouts = allRotations >>> mapM mockupToLayout

--------------------------------------------------------------------------------

labelBoxes :: Layout -> TaggedTree Attrs (Int, Box)
labelBoxes = (`evalState` 0) . cataM go where
  go :: TaggedTreeF Attrs Box (TaggedTree Attrs (Int, Box))
     -> State Int (TaggedTree Attrs (Int, Box))
  go (x :< Leaf a) = modify (+1) >> get >>= \i -> return (Fix $ x :< Leaf (i,a))
  go (x :< Branch b1 b2) = return $ Fix (x :< Branch b1 b2)

linkImages :: [Image] -> TaggedTree Attrs (Int, Box) -> Maybe Page
linkImages []   = const $ (Just EmptyPage)
linkImages imgs = fmap Page . cataM go where
  go (a :< Leaf (i,b))   = safeIx (i - 1) imgs >>= pure . Fix . (a :<) . Leaf
  go (a :< Branch b1 b2) = pure $ Fix $ a :< Branch b1 b2
  safeIx i xs
      | i <= length xs = pure $ xs !! i
      | otherwise      = Nothing

-- applyMockup :: [Image] -> Mockup1 -> Maybe Page
-- applyMockup imgs = mockupToLayout >=> applyLayout imgs

-- | Replace all boxes in a @Layout@ to produce a @Page@.
applyLayout :: MonadError LoggiaError m => Layout -> [Image] -> m Page
applyLayout _ []        = return EmptyPage
applyLayout layout imgs = case labelBoxes >>> (linkImages imgs) $ layout of
                            Nothing -> throwError NoMockupAvaliable
                            Just x  -> return x

-- selectMockup :: Ratio Int -> [Image]
--              -> LoggiaM Mockup1
-- selectMockup _ [] _              = do
--   debug "Empty list of layouts"
--   lift $ MaybeT $ return Nothing
-- selectMockup _ _  []             = do
--   debug "Empty list of images"
--   lift $ MaybeT $ return Nothing
-- selectMockup scale mockups imgs  = do
--   vs <- checkValidMockups
--   let l = min 3 $ length vs
--   let top3 = take l $ sortOn wastedArea vs
--   (!!) top3 <$> liftIO (randomRIO (0,l - 1))
--   where
--     imgsLength                   = fromIntegral (length imgs)
--     sameNumberOfElements mockup  = length ( mockup ^. boxes ) == imgsLength
--     wastedArea mockup            = sum $ zipWith go (mockup ^. boxes) imgs
--     validMockups                 = filter sameNumberOfElements mockups

--     checkValidMockups :: (Monad m) => LoggerT Log (MaybeT m) [Mockup1]
--     checkValidMockups = if length validMockups == 0
--                            then debug "Empty list of validMockups" >> lift (MaybeT . return $ Nothing)
--                            else return validMockups

--     go box img        = min (rB / rI) (rI / rB) where
--       rB = scale * aspectRatio box
--       rI = _imageAspectRatio img

-- | Gets a list of layouts with the same number of holes as the given list of
-- images.
askValidLayouts ::[Image] -> LoggiaT [Layout]
askValidLayouts imgs
  = (filter ((==) (length imgs) . countLayoutLeaves) <$> ask) >>= \case
      [] -> throwError NoMockupAvaliable
      xs -> return xs
-- | Given a list of images, a @Layout@ and its aspect ratio, calculate the
--
-- wastedArea :: Ratio Int -> [Image] -> Layout -> Int
-- wastedArea _ [] _ = 0
-- wastedArea scale imgs layouts = sum $ zipWith go $ listOfBoxes imgs
--   where
--     listOfBoxes = stripTag >>> flatten
--     go box img = min (rB / rI) (rI / rB) where
--       rB = scale * aspectRatio box
--       rI = _imageAspectRatio img

wastedImageArea :: Ratio Int -> Image -> Box -> Ratio Int
wastedImageArea scale img box = (area imageBox) - (area box')
  where
    box'     = box & horizontal . end *~ scale
    imgAR    = aspectRatio img
    boxAR    = aspectRatio box'
    imageBox = box & case compare imgAR boxAR of
             EQ -> id
             LT -> vertical   .~ Segment 0 (segmentSize (box ^. horizontal) / imgAR)
             GT -> horizontal .~ Segment 0 (segmentSize (box ^. vertical)   * imgAR)

wastedImagesArea :: Ratio Int -> [Image] -> Layout -> Ratio Int
wastedImagesArea scale imgs layout = sum $ zipWith (wastedImageArea scale) imgs bs
  where
    bs = stripTags >>> flatten $ layout

-- | Returns a random valid @Layout@.
selectRandomLayout :: [Image] -> LoggiaT Layout
selectRandomLayout []   = throwError NoImages
selectRandomLayout imgs = do
  layouts <- askValidLayouts imgs
  idx   <- getRandomR (0, length layouts - 1)
  return $ layouts !! idx

-- | Returns the most efficient @Layout@, where eficient means less image area
-- wasted. If there are multiple candidates, randomly selects one.
selectMostEfficientLayout :: Ratio Int -> [Image] -> LoggiaT Layout
selectMostEfficientLayout _ []       = throwError NoImages
selectMostEfficientLayout scale imgs = do
  layouts   <- askValidLayouts imgs
  let bestScore   = minimum $ wastedImagesArea scale imgs <$> layouts
  let bestLayouts = filter (wastedImagesArea scale imgs >>> (==) bestScore) layouts
  idx   <- getRandomR (0, length bestLayouts - 1)
  return $ bestLayouts !! idx

applyRandomLayout :: [Image] -> LoggiaT Page
applyRandomLayout []   = return $ EmptyPage
applyRandomLayout imgs = flip applyLayout imgs =<< selectRandomLayout imgs

arrange :: Ratio Int -> [[Image]] -> LoggiaT Album
arrange scale imgLists = do
  layouts <- (mapM (selectMostEfficientLayout scale) imgLists)
  pages   <- zipWithM applyLayout layouts imgLists
  return $ albumFromList scale pages

-- -- arrange :: Ratio Int -> [Layout] -> [[Image]] -> Maybe [Layout]
-- arrange :: (Monad m, MonadIO m) => Ratio Int -> [Mockup1] -> [[Image]] -> LoggerT Log (MaybeT m) [Page]
-- arrange scale mockups imgGroups = do
--   debug $ show imgGroups
--   debug $ show ( length mockups )
--   r <- listOfMaybeLayouts
--   lift $ MaybeT $ return $ zipWithM applyMockup imgGroups r
--   where
--     listOfMaybeLayouts = sequence ( selectMockup scale mockups <$> imgGroups)

--------------------------------------------------------------------------------

splitPage :: Int -> Page -> LoggiaT (Page, Page)
splitPage _ EmptyPage = return (EmptyPage, EmptyPage)
splitPage i (Page p)  = (,) <$> applyRandomLayout lImgs <*> applyRandomLayout rImgs
  where
    imgs           = stripTags >>> flatten $ p
    (lImgs, rImgs) = splitAt i imgs

leftSplitPageAt :: Int -> Album -> LoggiaT Album
leftSplitPageAt i album = do
  (newLeft, newCurrent) <- splitPage i $ album ^. currentPage
  return $ album
    & currentPage .~  newCurrent
    & leftPages   %~ (newLeft:)

rightSplitPageAt :: Int -> Album -> LoggiaT Album
rightSplitPageAt i album = do
  (newCurrent, newRight) <- splitPage i $ album ^. currentPage
  return $ album
    & currentPage .~  newCurrent
    & rightPages   %~ (newRight:)

--------------------------------------------------------------------------------

joinPages :: Page -> Page -> LoggiaT Page
joinPages EmptyPage p2        = return p2
joinPages p1 EmptyPage        = return p1
joinPages (Page p1) (Page p2) = applyRandomLayout (go p1 <> go p2)
  where go = stripTags >>> flatten

leftJoinPages :: Album -> LoggiaT Album
leftJoinPages a@(Album [] _ _ _) = return a
leftJoinPages (Album (l:ls) c rs ar ) = do
  nc <- joinPages l c
  return $ Album ls nc rs ar

rightJoinPages :: Album -> LoggiaT Album
rightJoinPages a@(Album _ _ [] _) = return a
rightJoinPages (Album ls c (r:rs) ar) = do
  nc <- joinPages c r
  return $ Album ls nc rs ar


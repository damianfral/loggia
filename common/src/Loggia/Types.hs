{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE OverloadedStrings  #-}

module Loggia.Types where

import           BasicPrelude              hiding (Foldable, (<>))
import           Control.Arrow             ((>>>))
import           Control.Lens              hiding ((:<), (.=))
import           Control.Monad.Except
import           Control.Monad.Random
import           Control.Monad.Reader
import           Control.Monad.State       hiding (fix)
import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Generics.Fixplate
import           Data.Ratio
import           Data.Semigroup
import           GHC.Generics
import           Loggia.Logger
import qualified Data.Text as T
--------------------------------------------------------------------------------

class Rectangle e where
  width  :: e -> Ratio Int
  height :: e -> Ratio Int

aspectRatio :: Rectangle e => e -> Ratio Int
aspectRatio e = width e / height e

area :: Rectangle e => e -> Ratio Int
area e = width e * height e

-- | A segment in a 1D @a@-space.
data SegmentF a = Segment
  { _start :: a
  , _end   :: a
  } deriving (Show, Read, Ord, Eq, Functor, Generic)

makeLenses ''SegmentF

segmentSize :: (Num a) => SegmentF a -> a
segmentSize (Segment a b) = abs $ b - a

-- | A type for "exact" segments.
type Segment = SegmentF (Ratio Int)

-- | A 'SegmentF a' is a Semigroup where @s1 <> s2@ produces the shortest
-- segment enclosings @s1@ and @s2@.
instance (Num a, Ord a) => Semigroup (SegmentF a) where
  s1 <> s2 = Segment
    ( min (s1 ^. start ) (s2 ^. start ) )
    ( max (s1 ^. end   ) (s2 ^. end   ) )

--------------------------------------------------------------------------------

-- | A @'BoxF' a@ is the product of 2 orthogonal @'SegmentF' a@ values.
data BoxF a = Box
  { _horizontal :: SegmentF a
  , _vertical   :: SegmentF a
  } deriving (Show, Read, Ord, Eq, Functor, Generic)

makeLenses ''BoxF

-- | A @'BoxF' a@ is a 'Semigroup' where @box1 '<>' box2@ produces the minimal
-- bounding box of @b1@ and @b2@.
instance (Num a, Ord a) => Semigroup (BoxF a) where
  (Box h1 v1) <> (Box h2 v2) = Box (h1 <> h2) (v1 <> v2)


left, right, top, bottom :: Lens' (BoxF a) a
left   = horizontal . start
right  = horizontal . end
top    = vertical   . start
bottom = vertical   . end

-- | A type for "exact" boxes.
type Box = BoxF (Ratio Int)

instance Rectangle Box where
  width  = segmentSize . _horizontal
  height = segmentSize . _vertical

-- | @normalizeBox w h b@ transforms a box b defined over a "canvas" with size
-- @w x h@ into a box over a @1x1@ canvas.
normalizeBox :: Fractional a => a -> a -> BoxF a -> BoxF a
normalizeBox w h b = b & horizontal %~ fmap (/ w) & vertical   %~ fmap (/ h)

--------------------------------------------------------------------------------

-- | 2 empty datatypes used for phantom "tagging".
data Normalized    = Normalized    deriving (Show, Read, Ord, Eq, Generic)
data NonNormalized = NonNormalized deriving (Show, Read, Ord, Eq, Generic)

-- | A @'Mockup' n@ is a set of boxes in a canvas with size @mockupWidth x
-- mockukpHeight@ tagged with a phantom type 'n'.
data MockupF n = Mockup
  { _boxes        :: [Box]
  , _mockupWidth  :: Int
  , _mockupHeight :: Int
  } deriving (Show, Read, Ord, Eq, Generic)

makeLenses ''MockupF

instance Rectangle (MockupF n) where
  width  = fromIntegral . _mockupWidth
  height = fromIntegral . _mockupHeight

type Mockup n = MockupF n
-- | A type for normalized mockups, mockups which canvas has size @1x1@.
type Mockup1 = Mockup Normalized

-- | Transform a non normalized mockup into a normalized one.
normalizeMockup :: Mockup NonNormalized -> Mockup Normalized
normalizeMockup (Mockup bs 1 1) = Mockup bs 1 1
normalizeMockup (Mockup bs w h) = Mockup (normalizeBox w' h' <$> bs) 1 1
  where [ w', h' ] = fromIntegral <$> [ w, h ]

--------------------------------------------------------------------------------

-- | A @'BinaryTreeF' a r@ is a binary tree with leafs containing @a@ values and
-- branches with two @r@ values. This @r@ values are the result of factorizing
-- the recursive stepping of a binary tree.
data BinaryTreeF a r = Leaf a | Branch r r deriving (Generic)

-- | A bunch of autoderived instances.
deriving instance (Show a, Show r) => Show (BinaryTreeF a r)
deriving instance (Read a, Read r) => Read (BinaryTreeF a r)
deriving instance (Eq   a, Eq   r) => Eq   (BinaryTreeF a r)
deriving instance (Ord  a, Ord  r) => Ord  (BinaryTreeF a r)
deriving instance Functor     (BinaryTreeF a)
deriving instance Foldable    (BinaryTreeF a)
deriving instance Traversable (BinaryTreeF a)

instance (Eq a) => EqF (BinaryTreeF a) where
  equalF = (==)
-- | A type synonym for the fixed point of @'BinaryTreeF' a r@.
type BinaryTree a     = Mu (BinaryTreeF a)

-- | Create a leaf of a @'BinaryTree' a@.
leaf :: a -> BinaryTree a
leaf = Fix . Leaf

-- | Create a branch  of a @'BinaryTree' a@.
branch :: BinaryTree a -> BinaryTree a -> BinaryTree a
branch b = Fix . Branch b

-- | A datatype to attach a tag to the nodes of a @'BinaryTreeF' a r@.
-- Note: Explore if this is or could be replaced by the cofree comonad.
data TaggedTreeF tag a r = tag :< BinaryTreeF a r
  deriving (Show, Read, Eq, Functor, Generic, Foldable, Traversable)

instance (Eq tag, Eq a) => EqF ( TaggedTreeF tag a ) where equalF = (==)

-- | Lens' to easily  work with the tag of 'TaggedTreeF's.
tag :: Lens' (TaggedTreeF t a r) t
tag f (x :< Leaf   a  ) = fmap (\n -> n :< Leaf a) (f x)
tag f (x :< Branch a b) = fmap (\n -> n :< Branch a b) (f x)

-- | A type synonym for the fixed point of @'TaggedTreeF' a@.
type TaggedTree tag a = Mu (TaggedTreeF tag a)

-- | Remove tag from a node..
stripTag :: TaggedTreeF tag a r -> BinaryTreeF a r
stripTag (_ :< b) = b

--------------------------------------------------------------------------------

-- | An F-Algebra like explained
-- <https://www.schoolofhaskell.com/user/bartosz/understanding-algebras here>.
type Algebra f a      = (Functor f) => (f a -> a)

-- | A @'Reducer' a@ is a evaluator for pattern functor
-- @'TaggedTreeF' 'Attrs' 'Box' r@.
-- Mostly for folding purposes.
type Reducer a        = Algebra (TaggedTreeF Attrs Box) a

-- | A Tagger is a 'Reducer' that produces a @'TaggedTreeF' 'Attrs' 'Box' r@..
type Tagger           = Reducer (TaggedTree  Attrs Box)

-- | Bottom-up traverse a @'Mu' f@ (fixed point of f) using an @'Algebra' f a@ to
-- produce an @a@ value.
bottomUp :: Functor f => Algebra f a -> Mu f -> a
bottomUp alg = unFix >>> fmap (bottomUp alg) >>> alg

-- | Remove tags from a @'TaggedTree' t a@.
stripTags :: TaggedTree t a -> BinaryTree a
stripTags = cata $ stripTag >>> Fix

-- | Create a list from a tree using its leafs.
flatten :: BinaryTree a -> [a]
flatten = cata go where
  go (Leaf x)     = [x]
  go (Branch f g) = f <> g

-- | Count all nodes in a @'BinaryTree' x@.
countNodes :: BinaryTree x -> Int
countNodes = cata go where
  go (Leaf _)     = 1
  go (Branch f g) = 1 + f + g

countLeaves :: BinaryTree x -> Int
countLeaves = cata go where
  go (Leaf _)     = 1
  go (Branch f g) = f + g

--------------------------------------------------------------------------------

data Direction = Horizontal | Vertical
  deriving (Show, Read, Eq, Ord, Generic)

-- | A datatype to be used as the tag for 'Layout' trees.
data Attrs = Attrs
  { _direction   :: Maybe Direction -- ^ Leafs doesn't have direction.
  , _boundingBox :: Box
  , _margins     :: Box
  , _relSize     :: Ratio Int
  , _totalArea   :: Ratio Int
  } deriving (Show, Read, Eq, Ord, Generic)

makeLenses ''Attrs

-- ????
emptyAttrs :: Attrs
emptyAttrs = Attrs Nothing undefined undefined 1 0

-------------------------------------------------------------------------------

-- | An @'ImageF' a@ represent a image with a path, an aspect ratio and also may
-- have some @a@ data.
data ImageF a = ImageF
  { _path        :: Text
  , _imageWidth  :: Int
  , _imageHeight :: Int
  , _imgData     :: Maybe a
  } deriving (Show, Read, Eq, Ord, Generic, Functor)

makeLenses ''ImageF

instance Rectangle (ImageF a) where
  width  = fromIntegral . _imageWidth
  height = fromIntegral . _imageHeight

-- | A type synonym for images without data.
type Image = ImageF ()

--------------------------------------------------------------------------------

-- | A 'Layout' is a binary tree with a 'Box' in the leafs and 'Attrs' in all
-- nodes. It comes from the binary partition of a 'Mockup'.
type Layout = TaggedTree Attrs Box

-- | A 'Page' is similar to a 'Layout', where the boxes are replaced with
-- images.
data Page  = EmptyPage
           | Page (TaggedTree Attrs Image) deriving Eq
-- type SPage  = SimpleTaggedTree Attrs Image

countLayoutLeaves :: TaggedTree t a -> Int
countLayoutLeaves = stripTags >>> countLeaves

countPageLeaves :: Page -> Int
countPageLeaves EmptyPage = 0
countPageLeaves (Page tt) = countLayoutLeaves tt

--------------------------------------------------------------------------------

-- | Display a '@BinaryTree' a@ (debugging purposes).
showTree :: (Show a) => BinaryTree a -> Text
showTree = cata go where
  go (Leaf x)       = T.pack $ show x
  go (Branch b1 b2) = unwords [ "(", showT b1, "|", showT b2, ")" ]
  showT = T.pack . show

--------------------------------------------------------------------------------

data Album = Album
  { _leftPages        :: [Page]
  , _currentPage      :: Page
  , _rightPages       :: [Page]
  , _albumAspectRatio :: Ratio Int
  } deriving (Generic)

makeLenses ''Album

instance Rectangle Album where
  width  = fromIntegral . numerator   . _albumAspectRatio
  height = fromIntegral . denominator . _albumAspectRatio

albumFromList :: Ratio Int -> [Page] -> Album
albumFromList ar []     = Album [] EmptyPage [] ar
albumFromList ar (x:xs) = Album [] x xs ar

albumToList :: Album -> [Page]
albumToList (Album ls c rs _) = reverse ls ++ [c] ++ rs

turnPageLeft :: Album -> Album
turnPageLeft a@(Album [] _ _ _) = a
turnPageLeft (Album (l:ls) c rs ar) = Album ls l (c:rs) ar

turnPageRight :: Album -> Album
turnPageRight a@(Album _ _ [] _)    = a
turnPageRight (Album ls c (r:rs) ar) = Album (c:ls) r rs ar

turnPagesLeft :: Int -> Album -> Album
turnPagesLeft i = foldl1 (.) (replicate i turnPageLeft)

turnPagesRight :: Int -> Album -> Album
turnPagesRight i = foldl1 (.) (replicate i turnPageRight)

--------------------------------------------------------------------------------

newtype LoggiaT a = LoggiaT
  { _runLoggiaT :: ReaderT [Layout] ( RandT StdGen ( ExceptT LoggiaError Identity))  a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadRandom
           , MonadReader [Layout]
           )

-- instance (MonadReader [Layout]) ( LoggiaT ) where
--   ask = LoggiaT $ ask

instance (MonadError LoggiaError) ( LoggiaT ) where
  throwError = LoggiaT . lift . lift . throwError

-- instance MonadRandom LoggiaT where
--   getRandomR = LoggiaT . lift . getRandomR

runLoggiaT :: StdGen -> [Layout] -> LoggiaT a -> Either LoggiaError a
runLoggiaT seed readerEnv
  =   _runLoggiaT
  >>> (flip runReaderT readerEnv)
  >>> flip evalRandT seed
  >>> runExceptT >>> runIdentity

data LoggiaError
  = NoMockupAvaliable
  | TooManyImages
  | NoImages

module Loggia.Box
  ( isInside
  , areOverlapping
  , getDirection
  , rotateBox
  , rotateBoxI
  , area
  , aspectRatio
  , calculateOffsets
  )
where

import           BasicPrelude   hiding ((<>))
import           Control.Arrow  ((>>>))
import           Control.Lens
import           Data.Ratio
import           Data.Semigroup
import           Loggia.Segment
import           Loggia.Types

--------------------------------------------------------------------------------

data BoxCenter a = BoxCenter
  { _x     :: a
  , _y     :: a
  , _left  :: a
  , _right :: a
  , _top   :: a
  , _down  :: a
  } deriving (Show, Eq)

boxToBoxCenter :: Num a => (a, a) -> BoxF a -> BoxCenter a
boxToBoxCenter (x,y) (Box h@(Segment h1 h2) v@(Segment v1 v2))
  = BoxCenter x y left right top down
  where
    left  = x  - h1
    right = h2 - x
    top   = y  - v1
    down  = v2 - y

boxCenterToBox :: Num a => BoxCenter a -> BoxF a
boxCenterToBox (BoxCenter x y l r t b) = Box (Segment h1 h2) (Segment v1 v2)
  where
    h1 = x - l
    h2 = r + x
    v1 = y - t
    v2 = b + y

{-
    +---v1---+         +---h2---+
    |        |         |        |
    h1   c  h2   ->    v1   c  v2
    |        |         |        |
    +---v2---+         +---h1---+
-}

rotateBoxCenter :: BoxCenter a -> BoxCenter a
rotateBoxCenter (BoxCenter x y left right top down)
  = BoxCenter x y top down right left

isInside :: Box -> Box -> Bool
isInside b1 b2 = b1 <> b2 == b1

areOverlapping :: Box -> Box -> Bool
areOverlapping b1 b2 = (areSegmentsOverlapped `on` ( view horizontal ) ) b1 b2
                    && (areSegmentsOverlapped `on` ( view vertical )   ) b1 b2

-- | Produces @Vertical@ when the 2 boxes are in the same column,
-- otherwise produces @Horizontal@ (should it produce a @Maybe Direction@ to
-- cover the case of overlapped boxes?).
getDirection :: Box -> Box -> Direction
getDirection b1 b2
    | (areSegmentsOverlapped `on` (view horizontal)) b1 b2 = Vertical
    | otherwise = Horizontal

calculateOffsets :: Box -> Box -> Box
calculateOffsets
  (Box (Segment l1 r1) (Segment t1 b1))
  (Box (Segment l2 r2) (Segment t2 b2)) = box
  where box = Box (Segment (l2 - l1) (r1 - r2)) (Segment (t2 - t1) (b1 - b2))

-- | Rotate a box 90 degress counter clockwise over a given center.
rotateBox :: (Num a) => (a,a) -> BoxF a -> BoxF a
rotateBox centerPoint = boxToBoxCenter centerPoint
                    >>> rotateBoxCenter >>> boxCenterToBox

-- | Apply @rotateBox@ @n@ times.
rotateBoxI :: (Num a) => Int -> (a, a) -> BoxF a -> BoxF a
rotateBoxI n c = foldr1 (>>>) $ replicate n (rotateBox c)

-- -- | Area of a box.
-- area :: Num a => BoxF a -> a
-- area b = ((*) `on` segmentSize) (b ^. horizontal) (b ^. vertical)

-- -- | Aspect ratio of a box.
-- aspectRatio :: Box -> Ratio Integer
-- aspectRatio b = ((/) `on` segmentSize) (b ^. horizontal) (b ^. vertical)



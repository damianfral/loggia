module Loggia.Mockup where

import           BasicPrelude           hiding (log)
import           Control.Arrow          ((>>>))
import           Control.Lens
import           Data.Generics.Fixplate
import           Data.Ratio
import           Loggia.Box
import           Loggia.Segment         as Seg
import           Loggia.Types
import           Loggia.Utils

--------------------------------------------------------------------------------

-- | Rotate a mockup (counter clockwise) around its center.
rotateMockup :: Mockup1 -> Mockup1
rotateMockup m = m & boxes  %~ fmap (rotateBox (0.5,0.5))

-- | Apply 'rotateMockup' @n@ times.
rotateMockupI :: Int -> Mockup1 -> Mockup1
rotateMockupI n = foldr1 (>>>) $ replicate n rotateMockup

-- | Rotate a mockup (clockwise) around its center.
rotateMockupL :: Mockup1 -> Mockup1
rotateMockupL = rotateMockupI 3

-- | Concatenate mockups.
concatenateHorizontal, concatenateVertical :: Mockup1 -> Mockup1 -> Mockup1
concatenateHorizontal (Mockup bs1 _ _) (Mockup bs2 _ _) = normalizeMockup
    $ Mockup (bs1 <> (go <$> bs2)) 2 1
    where go (Box h v) = Box (fmap (+1) h) v

concatenateVertical x = rotateMockupL . (concatenateHorizontal `on` rotateMockup) x

-- | Needs work!!!
generator :: Mockup1 -> Mockup1 -> Mockup1
generator x y = z
    where z = concatenateHorizontal x y

allRotations :: Mockup1 -> [Mockup1]
allRotations = nub . take 4 . iterate rotateMockup

--------------------------------------------------------------------------------

testMockup0 :: Mockup Normalized
testMockup0 = normalizeMockup $ Mockup [a,b,c] 1 1 where
  a = Box unitSegment     (Segment 0 0.5)
  b = Box (Segment 0 0.5) (Segment 0.5 1)
  c = Box (Segment 0.5 1) (Segment 0.5 1)

testMockup1 :: Mockup1
testMockup1  = normalizeMockup $ Mockup [a,b,b',c,d,d'] 10 20
  where
    a  = Box (Segment   0  10) (Segment  0  9)
    b  = Box (Segment 0.5   2) (Segment 10 14)
    b' = Box (Segment 0.5   2) (Segment 15 19)
    c  = Box (Segment 2.5 7.5) (Segment 10 19)
    d  = Box (Segment   8 9.5) (Segment 10 14)
    d' = Box (Segment   8 9.5) (Segment 15 19)

--------------------------------------------------------------------------------

-- | Transform all boxes in a mockup into segments using a function and
-- get the "space segments" between them.
spaceSegmentsInBetweenBoxesOn :: (Box -> Segment) -> Mockup1 -> [Segment]
spaceSegmentsInBetweenBoxesOn boxToSegment m = nub $ go initialSeg segments
  where
    initialSeg   = [Segment 0 1]
    segments     = boxToSegment <$> m ^. boxes
    go rs []     = rs
    go rs (x:xs) = go (rs >>= (`minusSegment` x)) xs

verticalSpaces, horizontalSpaces :: Mockup1 -> [Segment]
verticalSpaces   = spaceSegmentsInBetweenBoxesOn _horizontal
horizontalSpaces = spaceSegmentsInBetweenBoxesOn _vertical

verticalDivisors, horizontalDivisors :: Mockup1 -> [Ratio Int]
verticalDivisors   = fmap segmentCenter . verticalSpaces
horizontalDivisors = fmap segmentCenter . horizontalSpaces

-- | @'splitVMockup' m x@, if it's  possible,  splits a mockup @m@ into 2
-- mockups, one with boxes -- above horizontal line @x@ and the other with boxes
-- under it. Some divisions may be not possible.
splitVMockup :: Mockup1 -> Ratio Int-> Maybe (Mockup1, Mockup1)
splitVMockup _ 0 = Nothing
splitVMockup _ 1 = Nothing
splitVMockup m x = guard (validSplit split) >> return split
  where
    leftBox  = Box (Segment 0 x) (Segment 0 1)
    rightBox = Box (Segment x 1) (Segment 0 1)
    m1       = m & boxes %~ filter (isInside leftBox )
    m2       = m & boxes %~ filter (isInside rightBox)
    split    = (m1,m2)
    validSplit (l, r) = (length $ m ^. boxes) == ((length $ l ^. boxes) + (length $ r ^. boxes))
      && length (l ^. boxes) > 0
      && length (r ^. boxes) > 0

-- | Same as 'splitVMockup' but @x@ is a vertical line.
splitHMockup :: Mockup1 -> Ratio Int -> Maybe (Mockup1, Mockup1)
splitHMockup m x = do
  (l, r) <- splitVMockup (rotateMockup m) x
  return (rotateMockupL l, rotateMockupL r)

-- | Try to transform a normalized mockup into a @'BinaryTree' 'Box'@ through
-- binary partition.
unflatten :: Mockup1 -> Maybe (BinaryTree Box)
unflatten (Mockup [ ] _ _) = Nothing
unflatten (Mockup [x] _ _) = Just $ Fix $ Leaf x
unflatten m = Fix <$> (Branch <$> l <*> r)
  where
    balance (x,y)    = abs $ ((-) `on` (view boxes >>> length)) x y
    verticalSplits   = catMaybes $ splitVMockup m <$> (verticalDivisors   m)
    horizontalSplits = catMaybes $ splitHMockup m <$> (horizontalDivisors m)
    allSplits        = horizontalSplits <> verticalSplits
    bestSplit        = minimumOn balance allSplits
    l                = bestSplit >>= (fst >>> unflatten)
    r                = bestSplit >>= (snd >>> unflatten)

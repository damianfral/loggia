{-# LANGUAGE ViewPatterns         #-}

module Loggia.Segment where

import           BasicPrelude   hiding ((<>))
import           Control.Lens
import           Data.Semigroup
import           Loggia.Types

--------------------------------------------------------------------------------

unitSegment :: Segment
unitSegment = Segment 0 1

isSegmentInside :: Segment -> Segment -> Bool
isSegmentInside s1 s2 = s1 <> s2 == s1

areSegmentsOverlapped :: Segment -> Segment -> Bool
areSegmentsOverlapped s1 s2 = segmentSize (s1 <> s2) <= (segmentSize s1 + segmentSize s2)

segmentCenter :: (Num a, Fractional a) => SegmentF a -> a
segmentCenter (Segment a b) = (a + b) / 2

--   |-----|
--          |-----|
-- = |-----|

--    |---|
-- - |-----|
-- = ||

--   |-----|
-- -   |-|
-- = |-| |-|


--   |-----|
-- -   |-----|
-- = |-|


--     |-----|
-- - |-----|
-- =       |-|

-- | @'minusSegment' s1 s2@ removes the @s2@ segment from the @s1@ producing 0, 1 or 2
-- new segments.
minusSegment :: Segment -> Segment -> [Segment]
minusSegment s1 s2
  -- When s1 and s2 are not overlapped, produce [s1].
  | not (areSegmentsOverlapped s1 s2) = [ s1 ]
  -- When 's1' is inside 's2', produce '[]'.
  | isSegmentInside s2 s1 = mempty
  -- When s2 is inside s1, produce 2 segments. One of this segments could have
  -- length 0.
  | isSegmentInside s1 s2 = [ s1 & end   .~ (s2 ^. start) , s1 & start .~ (s2 ^. end) ]
  -- When s2 overlaps s1 to the right, return the left side of s1.
  | ((<) `on` (view start)) s1 s2 = [ s1 & end .~ (s2 ^. start) ]
  -- s2 overlaps s1 to the left, return the right side of s1.
  | otherwise = [ s1 & start .~ (s2 ^. end) ]

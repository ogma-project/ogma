module Web.Ogma.Api
  ( AbsoluteTime
  , absToInt
  , intToAbs
  , TimeInterval
  , boundedTime
  , before
  , after
  , exact
  , union
  , overlap
  ) where

newtype AbsoluteTime = AbsoluteTime { unAbsT ::  Int }
  deriving (Eq, Ord)

intToAbs :: Int -> AbsoluteTime
intToAbs = AbsoluteTime

absToInt :: AbsoluteTime -> Int
absToInt = unAbsT

instance Show AbsoluteTime where
  show (AbsoluteTime t) = show t

data TimeInterval = BoundedTime AbsoluteTime AbsoluteTime
                  | Before AbsoluteTime
                  | After AbsoluteTime
                  | Union TimeInterval TimeInterval
                  | Exact AbsoluteTime

boundedTime :: AbsoluteTime -> AbsoluteTime -> TimeInterval
boundedTime x y
  | x == y = Exact x
  | otherwise = BoundedTime (min x y) (max x y)

before :: AbsoluteTime -> TimeInterval
before = Before

after :: AbsoluteTime -> TimeInterval
after = After

exact :: AbsoluteTime -> TimeInterval
exact = Exact

union :: TimeInterval -> TimeInterval -> TimeInterval
union = Union

instance Show TimeInterval where
  show (BoundedTime begin end) = "[" ++ show begin ++ "; " ++ show end ++ "]"
  show (After begin) = "[" ++ show begin ++ "; inf]"
  show (Before end) = "[inf; " ++ show end ++ "]"
  show (Union int int') = "(" ++ show int ++ ") U (" ++ show int' ++ ")"
  show (Exact t) = "at " ++ show t

overlap :: TimeInterval -> TimeInterval -> Bool
overlap (BoundedTime begin end) (BoundedTime begin' end') = begin' < end && begin < end'
overlap (BoundedTime begin _) (Before end') = begin < end'
overlap (BoundedTime _ end) (After begin') = end < begin'
overlap (Before end) (After begin) = begin < end
overlap (Before _) (Before _) = True
overlap (After _) (After _) = True
overlap (Exact t) (BoundedTime begin end) = begin < t && t < end
overlap (Exact t) (Before end) = t < end
overlap (Exact t) (After begin) = begin < t
overlap (Exact t) (Exact t') = t == t'
overlap t (Union int int') = (overlap t int) || (overlap t int')
-- overlap is a symmetric relation, so we can simplify its implementation by
-- omitting half of the pattern matching cases and revert the arguments
overlap x y = overlap y x

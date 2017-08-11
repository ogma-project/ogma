{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}

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
  , readInterval
  , Point(..)
  , Surface
  , circle
  , point
  , triangle
  , rectangle
  , polygon
  , collide
  , Event(..)
  , filterEvents
  ) where

import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

newtype AbsoluteTime = AbsoluteTime { unAbsT ::  Int }
  deriving (Eq, Ord, Num, Enum, Real)

deriving instance Integral AbsoluteTime

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
  deriving (Eq)

readInterval :: String -> Maybe TimeInterval
readInterval str = parseMaybe parseInterval str
  where
    parseInterval :: Parser TimeInterval
    parseInterval = space *> (try parseBoundedTime
                              <|> try parseBefore
                              <|> try parseAfter
                              <|> parseExact
                              <|> parseUnion) <* space

    parseBoundedTime =
      boundedTime <$> (char '[' *> space *> L.signed space L.decimal <* space)
                  <*> (char ';' *> space *> L.signed space L.decimal <* space <* char ']')

    parseBefore =
      before <$> (char '[' *> space *> string "inf" *> space *> char ';' *> space *> L.signed space L.decimal
                    <* space <* char ']')

    parseAfter =
      after <$> (char '[' *> space *> L.signed space L.decimal <* space <* char ';' <* space <* string "inf"
                    <* space <* char ']')

    parseExact = exact <$> (string "at" *> space *> L.signed space L.decimal)

    parseUnion =
      union <$> (char '(' *> space *> parseInterval <* space <* char ')' <* space <* char 'U' <* space)
            <*> (char '(' *> space *> parseInterval <* space <* char ')')

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

data Point = Point Double Double
  deriving (Show)

data Surface = Polygon Point Point Point [Point]
             | Circle Point Double
  deriving (Show)

circle :: Point -> Double -> Surface
circle p r = Circle p (abs r)

point :: Point -> Surface
point p = circle p 0.0

polygon :: Point -> Point -> Point -> [Point] -> Surface
polygon = Polygon

triangle :: Point -> Point -> Point -> Surface
triangle p1 p2 p3 = polygon p1 p2 p3 []

rectangle :: Point -> Point -> Surface
rectangle (Point x y) (Point x' y') = polygon (Point (min x x') (min y y'))
                                              (Point (min x x') (max y y'))
                                              (Point (max x x') (max y y'))
                                              [Point (max x x') (min y y')]

data Box = Box Point Point

type Title = String
type Description = String

collide :: Surface -> Surface -> Bool
collide s1 s2 = boxCollide (surfaceToBox s1) (surfaceToBox s2)
  where
    boxCollide :: Box -> Box -> Bool
    boxCollide (Box (Point xmin1 ymin1) (Point xmax1 ymax1))
               (Box (Point xmin2 ymin2) (Point xmax2 ymax2)) =
      not ((xmin2 > xmax1) ||
           (xmin1 > xmax2) ||
           (ymin2 > ymax1) ||
           (ymin1 > ymax2))

surfaceToBox :: Surface -> Box
surfaceToBox (Circle (Point x y) r) =
  Box (Point (x - r) (y - r)) (Point (x + r) (y + r))
surfaceToBox (Polygon p p' p'' r) = foldl expand (Box p p) (p':p'':r)
  where
    expand :: Box -> Point -> Box
    expand (Box (Point minx miny) (Point maxx maxy)) (Point x y) =
      Box (Point (min minx x) (min miny y))
          (Point (max maxx x) (max maxy y))

data Event = Event Title Description TimeInterval Surface

selectEvent :: TimeInterval -> Surface -> Event -> Bool
selectEvent int' surface' (Event _ _ int surface) = (overlap int int') && collide surface surface'

filterEvents :: TimeInterval -> Surface -> [Event] -> [Event]
filterEvents int surface = filter $ selectEvent int surface

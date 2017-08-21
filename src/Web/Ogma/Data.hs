{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}

module Web.Ogma.Data
  ( Title
  , Name
  , Description
  , AbsoluteTime
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
  , readSurface
  , circle
  , point
  , triangle
  , rectangle
  , polygon
  , collide
  , Location(..)
  , filterEvents
  ) where

import           Data.Aeson
import           Data.String
import           Data.Text                  (Text, append, pack, unpack)
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Web.HttpApiData

type Parser = Parsec Void String

newtype AbsoluteTime = AbsoluteTime { unAbsT ::  Int }
  deriving (Eq, Ord, Num, Enum, Real, FromJSON, ToJSON)

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

readInterval :: String -> Maybe TimeInterval
readInterval = parseMaybe parseInterval
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

instance ToHttpApiData TimeInterval where
  toQueryParam = pack . show

instance FromHttpApiData TimeInterval where
  parseQueryParam txt = case readInterval (unpack txt) of
    Just x -> Right x
    _      -> Left $ "couldn't parse an interval from " `append` txt


instance ToJSON TimeInterval where
  toJSON txt = String $ fromString (show txt)

instance FromJSON TimeInterval where
  parseJSON (String txt) = case readInterval (unpack txt) of
    Just x  -> pure x
    Nothing -> fail "incorrect time interval"

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
overlap t (Union int int') = overlap t int || overlap t int'
-- overlap is a symmetric relation, so we can simplify its implementation by
-- omitting half of the pattern matching cases and revert the arguments
overlap x y = overlap y x

data Point = Point Double Double
  deriving (Eq)

instance Show Point where
  show (Point x y) = "(" ++ show x ++ ";" ++ show y ++ ")"

instance ToJSON Point where
  toJSON (Point x y) = object [ "x" .= x
                              , "y" .= y
                              ]
instance FromJSON Point where
  parseJSON (Object o) = Point <$> o .: "x"
                               <*> o .: "y"

data Surface = Polygon Point Point Point [Point]
             | Circle Point Double
  deriving (Eq)

instance Show Surface where
  show (Polygon p p' p'' rest) =
    "poly:" ++ show p ++ "-" ++ show p' ++ "-" ++ show p'' ++ showRest rest
    where showRest (x:r) = "-" ++ show x ++ showRest r
          showRest []    = ""
  show (Circle c r) = "circle:" ++ show c ++ "-" ++ show r

instance ToHttpApiData Surface where
  toQueryParam = pack . show

readSurface :: String -> Maybe Surface
readSurface = parseMaybe parseSurface
  where
    parseSurface :: Parser Surface
    parseSurface = (do string "poly:"
                       p <- parsePoint
                       char '-'
                       p' <- parsePoint
                       char '-'
                       p'' <- parsePoint
                       rest <- many (char '-' *> parsePoint)
                       pure $ Polygon p p' p'' rest)
      <|> (do string "circle:"
              c <- parsePoint
              char '-'
              r <- L.float

              pure $ Circle c r)

    parsePoint :: Parser Point
    parsePoint = Point <$> (char '(' *> L.signed space L.float <* char ';')
                       <*> (L.signed space L.float <* char ')')

instance FromHttpApiData Surface where
  parseQueryParam txt = case readSurface (unpack txt) of
    Just x -> Right x
    _      -> Left $ "couldn't parse a surface from " `append` txt

instance ToJSON Surface where
  toJSON (Polygon p p' p'' r) = object [ "polygon" .= toJSON (p:p':p'':r)
                                       ]
  toJSON (Circle p r) = object [ "center" .= p
                               , "radius" .= r
                               ]

instance FromJSON Surface where
  parseJSON (Object o) = (do points <- o .: "polygon"
                             case points of
                               (p:p':p'':r) -> pure $ Polygon p p' p'' r
                               _ -> fail "polygon should be made of at least three points")
                         <|> (Circle <$> o .: "center"
                                     <*> o .: "radius")

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

type Title = Text
type Name = Text
type Description = Text

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

data Location = Location Title Description Surface
  deriving (Eq, Show)

instance ToJSON Location where
  toJSON (Location n d s) = object [ "name" .= n
                                   , "description" .= d
                                   , "surface" .= s
                                   ]
instance FromJSON Location where
  parseJSON (Object o) = Location <$> o .: "name"
                                  <*> o .: "description"
                                  <*> o .: "surface"

selectEvent :: Maybe TimeInterval -> Maybe Surface -> (a -> TimeInterval) -> (a -> Surface) -> a -> Bool
selectEvent (Just int') (Just surface') getInt getSurface x = overlap (getInt x) int' && collide (getSurface x) surface'
selectEvent Nothing (Just surface') _ getSurface x = collide (getSurface x) surface'
selectEvent (Just int') Nothing getInt _ x = overlap (getInt x) int'
selectEvent Nothing Nothing _ _ x = True

filterEvents :: Maybe TimeInterval -> Maybe Surface -> (a -> TimeInterval) -> (a -> Surface) -> [a] -> [a]
filterEvents Nothing Nothing _ _ = id
filterEvents int surface getInt getSurface = filter $ selectEvent int surface getInt getSurface

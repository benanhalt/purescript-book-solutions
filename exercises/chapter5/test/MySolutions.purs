module Test.MySolutions where

import Prelude
import Data.Number (pi)
import Data.Maybe (Maybe(..))
import Data.Person (Person)
import Data.Picture (Shape(..), origin)
import ChapterExamples (Amp(..), Volt(..))

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

binomial :: Int -> Int -> Int
binomial 0 _ = 0
binomial _ 0 = 1
binomial n k | k > n = 0
binomial n k = factorial n / (factorial k) / (factorial (n - k))

pascal :: Int -> Int -> Int
pascal _ 0 = 1
pascal n k | k > n = 0
pascal n k = pascal (n - 1) k + pascal (n - 1) (k - 1)


sameCity :: Person -> Person -> Boolean
sameCity {address: {city: city1}} {address: {city: city2}} = city1 == city2

fromSingleton :: forall a. a -> Array a -> a
fromSingleton _ [a] = a
fromSingleton a _ = a

circleAtOrigin :: Shape
circleAtOrigin = Circle {x: 0.0, y: 0.0} 10.0

doubleScaleAndCenter :: Shape -> Shape
doubleScaleAndCenter (Clipped p c w h) =
  Clipped (doubleScaleAndCenter <$> p) origin (2.0*w) (2.0*h)
doubleScaleAndCenter (Circle _ r) = Circle origin (2.0*r)
doubleScaleAndCenter (Rectangle _ w h) = Rectangle origin (2.0*w) (2.0*h)
doubleScaleAndCenter (Text _ text) = Text origin text
doubleScaleAndCenter (Line {x: x1, y: y1} {x: x2, y: y2}) =
  Line {x: x1 - x2, y: y1 - y2} {x: x2 - x1, y: y2 - y1}

shapeText :: Shape -> Maybe String
shapeText (Text _ text) = Just text
shapeText _ = Nothing

newtype Watt = Watt Number

calculateWattage :: Amp -> Volt -> Watt
calculateWattage (Amp a) (Volt v) = Watt (a*v)

area :: Shape -> Number
area (Circle _ r) = pi*r*r
area (Rectangle _ w h) = w * h
area _ = 0.0

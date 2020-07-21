module Test.MySolutions where

import Prelude
import Data.Person
import Data.Picture
import Data.Maybe(Maybe(..))
import Data.Foldable (foldl)
import Global as Global
import Math as Math

factorial :: Int -> Int
factorial 0 = 1
factorial 1 = 1
factorial n = n * factorial (n - 1)

binomial :: Int -> Int -> Int
binomial _ 0 = 1
binomial 0 _ = 0
binomial n k | n < k = 0
  | otherwise = factorial n / (factorial k * (factorial (n - k)))

pascal :: Int -> Int -> Int
pascal _ 0 = 1
pascal 0 _ = 0
pascal n k = pascal (n - 1) k + pascal (n -1) (k - 1)

sameCity :: Person -> Person -> Boolean
sameCity { address: { city: city1 } } { address: { city: city2 }} = city1 == city2

fromSingleton :: forall a. a -> Array a -> a
fromSingleton _ [a]  = a
fromSingleton a _ = a


circleAtOrigin :: Shape
circleAtOrigin = Circle origin 10.0

centerShape :: Shape -> Shape
centerShape (Circle _ r) = Circle origin r
centerShape (Rectangle _ x y)  = Rectangle origin x y
centerShape (Line (Point s) (Point e)) =
  (Line
    (Point { x: s.x - deltaX, y: s.y - deltaY })
    (Point { x: e.x - deltaX, y: e.y - deltaY })
  )
  where
  deltaX = (e.x + s.x) / 2.0
  deltaY = (e.y + s.y) / 2.0
centerShape (Text _ s) = Text origin s

scaleShape :: Number -> Shape -> Shape
scaleShape i (Circle c r) = Circle c (i * r)
scaleShape i (Rectangle c x y) = Rectangle c (i * x) (i * y)
scaleShape i (Line (Point s) (Point e)) =
  (Line
    (Point { x: s.x * i, y: s.y * i })
    (Point { x: e.x * i, y: e.y * i })
  )
scaleShape _ shape = shape

doubleScaleAndCenter :: Shape -> Shape
doubleScaleAndCenter = centerShape <<< scaleShape 2.0

shapeText :: Shape -> Maybe String
shapeText (Text _ s) = Just s
shapeText _ = Nothing

area :: Shape -> Number
area (Circle _ r) = r * r * Math.pi
area (Rectangle _ w h) = w * h
area _ = 0.0
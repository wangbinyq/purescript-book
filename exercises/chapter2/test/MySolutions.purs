module Test.MySolutions where

import Prelude
import Math (sqrt, pi, e)
import Global (readFloat)

diagonal x y = sqrt(x * x + y * y)

circleArea r = r * r * pi

addE s = readFloat s + e
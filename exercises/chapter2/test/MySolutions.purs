module Test.MySolutions where

import Prelude

import Data.Number (sqrt, pi)
import Data.Int (rem)

diagonal w h = sqrt (w * w + h * h)

circleArea r = pi * r * r

leftoverCents n = n `rem` 100
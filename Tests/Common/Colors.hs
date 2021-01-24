module Tests.Common.Colors where

import Utils.Image (Rgb (..))

equalColors :: Rgb -> Rgb -> Bool
equalColors rgb1 rgb2 = red rgb1 == red rgb2 && green rgb1 == green rgb2 && blue rgb1 == blue rgb2

testRedColor :: Rgb
testRedColor = Rgb 255 0 0
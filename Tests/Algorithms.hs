module Tests.Algorithms where

import Algorithms.Atkinson (atkinson)
import Test.HUnit (Test (TestCase), assertEqual)
import Tests.Common.Images
  ( testBlackWhiteImage,
    testGrayscaleImage,
    testRGBImage,
  )
import Utils.Image
  ( Image (Image, colors, content, format, height, width),
    Rgb (Rgb, blue, green, red),
  )

testAtkinson :: Test
testAtkinson = TestCase $ do
  assertEqual "Atkinson dithering works on bw" (atkinson testBlackWhiteImage) Image {format = 1, width = 10, height = 10, content = [[Rgb {red = 255, green = 255, blue = 255}, Rgb {red = 255, green = 255, blue = 255}, Rgb {red = 255, green = 255, blue = 255}, Rgb {red = 255, green = 255, blue = 255}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 255, green = 255, blue = 255}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 255, green = 255, blue = 255}, Rgb {red = 255, green = 255, blue = 255}], [Rgb {red = 255, green = 255, blue = 255}, Rgb {red = 255, green = 255, blue = 255}, Rgb {red = 255, green = 255, blue = 255}, Rgb {red = 255, green = 255, blue = 255}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 255, green = 255, blue = 255}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 255, green = 255, blue = 255}], [Rgb {red = 255, green = 255, blue = 255}, Rgb {red = 255, green = 255, blue = 255}, Rgb {red = 255, green = 255, blue = 255}, Rgb {red = 255, green = 255, blue = 255}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 255, green = 255, blue = 255}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 255, green = 255, blue = 255}], [Rgb {red = 255, green = 255, blue = 255}, Rgb {red = 255, green = 255, blue = 255}, Rgb {red = 255, green = 255, blue = 255}, Rgb {red = 255, green = 255, blue = 255}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 255, green = 255, blue = 255}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 255, green = 255, blue = 255}, Rgb {red = 255, green = 255, blue = 255}], [Rgb {red = 255, green = 255, blue = 255}, Rgb {red = 255, green = 255, blue = 255}, Rgb {red = 255, green = 255, blue = 255}, Rgb {red = 255, green = 255, blue = 255}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 255, green = 255, blue = 255}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 255, green = 255, blue = 255}, Rgb {red = 0, green = 0, blue = 0}], [Rgb {red = 255, green = 255, blue = 255}, Rgb {red = 255, green = 255, blue = 255}, Rgb {red = 255, green = 255, blue = 255}, Rgb {red = 255, green = 255, blue = 255}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 255, green = 255, blue = 255}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 255, green = 255, blue = 255}, Rgb {red = 255, green = 255, blue = 255}], [Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 255, green = 255, blue = 255}, Rgb {red = 255, green = 255, blue = 255}, Rgb {red = 255, green = 255, blue = 255}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 255, green = 255, blue = 255}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 255, green = 255, blue = 255}, Rgb {red = 255, green = 255, blue = 255}], [Rgb {red = 255, green = 255, blue = 255}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 255, green = 255, blue = 255}, Rgb {red = 255, green = 255, blue = 255}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}], [Rgb {red = 255, green = 255, blue = 255}, Rgb {red = 255, green = 255, blue = 255}, Rgb {red = 255, green = 255, blue = 255}, Rgb {red = 255, green = 255, blue = 255}, Rgb {red = 255, green = 255, blue = 255}, Rgb {red = 255, green = 255, blue = 255}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 255, green = 255, blue = 255}], [Rgb {red = 255, green = 255, blue = 255}, Rgb {red = 255, green = 255, blue = 255}, Rgb {red = 255, green = 255, blue = 255}, Rgb {red = 255, green = 255, blue = 255}, Rgb {red = 255, green = 255, blue = 255}, Rgb {red = 255, green = 255, blue = 255}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 255, green = 255, blue = 255}, Rgb {red = 0, green = 0, blue = 0}]], colors = 1}
  assertEqual "Atkinson dithering works on grayscale" (atkinson testGrayscaleImage) Image {format = 2, width = 24, height = 7, content = [[Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}], [Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 255, green = 255, blue = 255}, Rgb {red = 255, green = 255, blue = 255}, Rgb {red = 255, green = 255, blue = 255}, Rgb {red = 255, green = 255, blue = 255}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 255, green = 255, blue = 255}, Rgb {red = 255, green = 255, blue = 255}, Rgb {red = 255, green = 255, blue = 255}, Rgb {red = 255, green = 255, blue = 255}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 255, green = 255, blue = 255}, Rgb {red = 255, green = 255, blue = 255}, Rgb {red = 255, green = 255, blue = 255}, Rgb {red = 255, green = 255, blue = 255}, Rgb {red = 0, green = 0, blue = 0}], [Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 255, green = 255, blue = 255}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 255, green = 255, blue = 255}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 255, green = 255, blue = 255}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 255, green = 255, blue = 255}, Rgb {red = 0, green = 0, blue = 0}], [Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 255, green = 255, blue = 255}, Rgb {red = 255, green = 255, blue = 255}, Rgb {red = 255, green = 255, blue = 255}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 255, green = 255, blue = 255}, Rgb {red = 255, green = 255, blue = 255}, Rgb {red = 255, green = 255, blue = 255}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 255, green = 255, blue = 255}, Rgb {red = 255, green = 255, blue = 255}, Rgb {red = 255, green = 255, blue = 255}, Rgb {red = 255, green = 255, blue = 255}, Rgb {red = 0, green = 0, blue = 0}], [Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 255, green = 255, blue = 255}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 255, green = 255, blue = 255}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 255, green = 255, blue = 255}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}], [Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 255, green = 255, blue = 255}, Rgb {red = 255, green = 255, blue = 255}, Rgb {red = 255, green = 255, blue = 255}, Rgb {red = 255, green = 255, blue = 255}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 255, green = 255, blue = 255}, Rgb {red = 255, green = 255, blue = 255}, Rgb {red = 255, green = 255, blue = 255}, Rgb {red = 255, green = 255, blue = 255}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 255, green = 255, blue = 255}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}], [Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}]], colors = 255}
  assertEqual "Atkinson dithering works on RGB" (atkinson testRGBImage) Image {format = 3, width = 8, height = 7, content = [[Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}], [Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 255, blue = 255}, Rgb {red = 255, green = 255, blue = 0}, Rgb {red = 0, green = 255, blue = 255}, Rgb {red = 255, green = 255, blue = 0}, Rgb {red = 0, green = 255, blue = 255}, Rgb {red = 255, green = 255, blue = 0}], [Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 255, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 255, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 255, blue = 0}, Rgb {red = 0, green = 255, blue = 0}], [Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 255, blue = 255}, Rgb {red = 255, green = 0, blue = 0}, Rgb {red = 0, green = 255, blue = 255}, Rgb {red = 255, green = 0, blue = 0}, Rgb {red = 0, green = 255, blue = 255}, Rgb {red = 255, green = 255, blue = 0}], [Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 255, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 255, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 255, blue = 0}, Rgb {red = 0, green = 0, blue = 0}], [Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 255, blue = 255}, Rgb {red = 255, green = 255, blue = 0}, Rgb {red = 0, green = 255, blue = 255}, Rgb {red = 255, green = 255, blue = 0}, Rgb {red = 0, green = 255, blue = 0}, Rgb {red = 0, green = 0, blue = 0}], [Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}, Rgb {red = 0, green = 0, blue = 0}]], colors = 255}
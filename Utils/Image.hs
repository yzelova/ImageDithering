module Utils.Image where

import Data.Word (Word8)

data Rgb = Rgb
  { red :: Word8,
    green :: Word8,
    blue :: Word8
  }
  deriving (Read, Show)

data Image = Image
  { format :: Int,
    width :: Int,
    height :: Int,
    content :: [[Rgb]],
    colors :: Int
  }
  deriving (Read, Show)

rgbToGs :: Rgb -> Rgb
rgbToGs rgb =
  Rgb color color color
  where
    color = round (0.3 * fromIntegral (red rgb) + (0.59 * fromIntegral (green rgb)) + (0.11 * fromIntegral (blue rgb)))

grayscaleHelper :: [[Rgb]] -> [[Rgb]]
grayscaleHelper = Prelude.map $ Prelude.map rgbToGs

grayscale :: Image -> Image
grayscale img = Image (format img) (width img) (height img) (grayscaleHelper $ content img) (colors img)

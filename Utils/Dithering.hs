module Utils.Dithering where

import Data.Word (Word8)
import Utils.Image (Rgb (..))

isOutOfBound :: Int -> Int -> Int -> Int -> Bool
isOutOfBound w h row col
  | row < 0 = True
  | col < 0 = True
  | col >= w = True
  | row >= h = True
  | otherwise = False

setPixel :: Rgb -> (Int, Int) -> [[Rgb]] -> [[Rgb]]
setPixel newColor (0, 0) content = (newColor : drop 1 (head content)) : tail content
setPixel newColor (0, col) content = (take col (head content) ++ [newColor] ++ drop (col + 1) (head content)) : tail content
setPixel newColor (row, col) content = take row content ++ [take col (content !! row) ++ [newColor] ++ drop (col + 1) (content !! row)] ++ drop (row + 1) content

setPixelDithering :: Int -> Int -> Int -> Int -> Int -> Int -> (Word8, Word8, Word8) -> [[Rgb]] -> [[Rgb]]
setPixelDithering w h row col diffusion divisor (errorR, errorG, errorB) matrix
  | isOutOfBound w h row col = matrix
  | otherwise =
    let pixel = matrix !! row !! col
        r = red pixel
        g = green pixel
        b = blue pixel
        newRed = r + round (toRational (fromIntegral errorR * diffusion) / toRational divisor)
        newGreen = g + round (toRational (fromIntegral errorG * diffusion) / toRational divisor)
        newBlue = b + round (toRational (fromIntegral errorB * diffusion) / toRational divisor)
        closestRed
          | newRed > 255 = 0
          | newRed < 0 = 255
          | newRed <= 127 = 0
          | otherwise = 255
        closestGreen
          | newGreen > 255 = 0
          | newGreen < 0 = 255
          | newGreen <= 127 = 0
          | otherwise = 255
        closestBlue
          | newBlue > 255 = 0
          | newBlue < 0 = 255
          | newBlue <= 127 = 0
          | otherwise = 255
     in setPixel (Rgb closestRed closestGreen closestBlue) (row, col) matrix
setPixelOrderedDithering :: Int -> Int -> Int -> Int -> [[Int]] -> Int -> [[Rgb]] -> [[Rgb]]
setPixelOrderedDithering w h row col m n matrix
  | isOutOfBound w h row col = matrix
  | otherwise =
    let pixel = matrix !! row !! col
        r = red pixel
        g = green pixel
        b = blue pixel
        mX = row `mod` n
        mY = col `mod` n
        newRed = r + round (255 * (toRational (m !! mX !! mY) - 0.5))
        newGreen = g + round (255 * (toRational (m !! mX !! mY) - 0.5))
        newBlue = b + round (255 * (toRational (m !! mX !! mY) - 0.5))
        closestRed
          | newRed > 255 = 255
          | newRed < 0 = 0
          | newRed <= 127 = 0
          | otherwise = 255
        closestGreen
          | newGreen > 255 = 255
          | newGreen < 0 = 0
          | newGreen <= 127 = 0
          | otherwise = 255
        closestBlue
          | newBlue > 255 = 255
          | newBlue < 0 = 0
          | newBlue <= 127 = 0
          | otherwise = 255
     in setPixel (Rgb closestRed closestGreen closestBlue) (row, col) matrix

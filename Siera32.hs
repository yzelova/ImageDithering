module Burkes where
import Dithering ( setPixel, setPixelDithering )
import FileOperations ( loadImage, saveImage )
import Image ( Image(..), Rgb(..), grayscale )

applySierra32 :: Int -> Int -> Int -> Int -> [[Rgb]] -> [[Rgb]]
applySierra32 w h row col rgb
  | row == h - 1 && col == w - 1 = rgb
  | otherwise =
    let pixel = rgb !! row !! col
        r = red pixel
        g = green pixel
        b = blue pixel
        (newR, errorR)
          | r <= 127 = (0, r)
          | otherwise = (255, r - 255)
        (newG, errorG)
          | g <= 127 = (0, g)
          | otherwise = (255, g - 255)
        (newB, errorB)
          | b <= 127 = (0, b)
          | otherwise = (255, b - 255)
        newMatrix = setPixel (Rgb newR newG newB) (row, col) rgb
        matrixWithError1 = setPixelDithering w h row (col + 1) 5 32 (errorR, errorG, errorB) newMatrix
        matrixWithError2 = setPixelDithering w h row (col + 2) 3 32 (errorR, errorG, errorB) matrixWithError1
        matrixWithError3 = setPixelDithering w h (row + 1) (col - 2) 2 32 (errorR, errorG, errorB) matrixWithError2
        matrixWithError4 = setPixelDithering w h (row + 1) (col - 1) 4 32 (errorR, errorG, errorB) matrixWithError3
        matrixWithError5 = setPixelDithering w h (row + 1) col 5 32 (errorR, errorG, errorB) matrixWithError4
        matrixWithError6 = setPixelDithering w h (row + 1) (col + 1) 4 32 (errorR, errorG, errorB) matrixWithError5
        matrixWithError7 = setPixelDithering w h (row + 1) (col + 2) 2 32 (errorR, errorG, errorB) matrixWithError6
        matrixWithError8 = setPixelDithering w h (row + 2) (col - 1) 2 32 (errorR, errorG, errorB) matrixWithError7
        matrixWithError9 = setPixelDithering w h (row + 2) col 3 32 (errorR, errorG, errorB) matrixWithError8
        matrixWithError10 = setPixelDithering w h (row + 2) (col + 1) 2 32 (errorR, errorG, errorB) matrixWithError9
        newCol
          | col == w - 1 = 0
          | otherwise = col + 1
        newRow
          | col == w - 1 = row + 1
          | otherwise = row
    in applySierra32 w h newRow newCol matrixWithError10
    
sierra32 :: Image -> Image
sierra32 img = Image (format img) (width img) (height img) (applySierra32 (width img) (height img) 0 0 (content img)) (colors img)

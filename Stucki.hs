module Stucki where
import Dithering ( setPixel, setPixelDithering )
import FileOperations ( loadImage, saveImage )
import Image ( Image(..), Rgb(..), grayscale )

applyStucki :: Int -> Int -> Int -> Int -> [[Rgb]] -> [[Rgb]]
applyStucki w h row col rgb
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
        matrixWithError1 = setPixelDithering w h row (col + 1) 8 42 (errorR, errorG, errorB) newMatrix
        matrixWithError2 = setPixelDithering w h row (col + 2) 4 42 (errorR, errorG, errorB) matrixWithError1
        matrixWithError3 = setPixelDithering w h (row + 1) (col - 2) 2 42 (errorR, errorG, errorB) matrixWithError2
        matrixWithError4 = setPixelDithering w h (row + 1) (col - 1) 4 42 (errorR, errorG, errorB) matrixWithError3
        matrixWithError5 = setPixelDithering w h (row + 1) col 8 42 (errorR, errorG, errorB) matrixWithError4
        matrixWithError6 = setPixelDithering w h (row + 1) (col + 1) 4 42 (errorR, errorG, errorB) matrixWithError5
        matrixWithError7 = setPixelDithering w h (row + 1) (col + 2) 2 42 (errorR, errorG, errorB) matrixWithError6
        matrixWithError8 = setPixelDithering w h (row + 2) (col - 2) 1 42 (errorR, errorG, errorB) matrixWithError7
        matrixWithError9 = setPixelDithering w h (row + 2) (col - 1) 2 42 (errorR, errorG, errorB) matrixWithError8
        matrixWithError10 = setPixelDithering w h (row + 2) col 4 42 (errorR, errorG, errorB) matrixWithError9
        matrixWithError11 = setPixelDithering w h (row + 2) (col + 1) 2 42 (errorR, errorG, errorB) matrixWithError10
        matrixWithError12 = setPixelDithering w h (row + 2) (col + 2) 1 42 (errorR, errorG, errorB) matrixWithError11
        newCol
          | col == w - 1 = 0
          | otherwise = col + 1
        newRow
          | col == w - 1 = row + 1
          | otherwise = row
    in applyStucki w h newRow newCol matrixWithError12
    
stucki :: Image -> Image
stucki img = Image (format img) (width img) (height img) (applyStucki (width img) (height img) 0 0 (content img)) (colors img)

execute :: FilePath -> FilePath  -> IO ()
execute input output = do
  image <- loadImage input
  let newImage = stucki (grayscale image)
  saveImage output newImage
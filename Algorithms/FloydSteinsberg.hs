module Algorithms.FloydSteinsberg where
import Dithering ( setPixel, setPixelDithering )
import FileOperations ( loadImage, saveImage )
import Image ( Image(..), Rgb(..), grayscale )


applyFloydSteinsberg :: Int -> Int -> Int -> Int -> [[Rgb]] -> [[Rgb]]
applyFloydSteinsberg w h row col rgb
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
        matrixWithError1 = setPixelDithering w h row (col + 1) 7 16 (errorR, errorG, errorB) newMatrix
        matrixWithError2 = setPixelDithering w h (row + 1) (col - 1) 3 16 (errorR, errorG, errorB) matrixWithError1
        matrixWithError3 = setPixelDithering w h (row + 1) col 5 16 (errorR, errorG, errorB) matrixWithError2
        matrixWithError4 = setPixelDithering w h (row + 1) (col + 1) 1 16 (errorR, errorG, errorB) matrixWithError3
        newCol
          | col == w - 1 = 0
          | otherwise = col + 1
        newRow
          | col == w - 1 = row + 1
          | otherwise = row
    in applyFloydSteinsberg w h newRow newCol matrixWithError4

floydSteinberg :: Image -> Image
floydSteinberg img = Image (format img) (width img) (height img) (applyFloydSteinsberg (width img) (height img) 0 0 (content img)) (colors img)

execute :: FilePath -> FilePath  -> IO ()
execute input output = do
  image <- loadImage input
  let newImage = floydSteinberg (grayscale image)
  saveImage output newImage
module Algorithms.FalseFloydSteinberg where

import Utils.Dithering (setPixel, setPixelDithering)
import Utils.FileOperations (loadImage, saveImage)
import Utils.Image (Image (..), Rgb (..), grayscale)

applyFalseFloydSteinsberg :: Int -> Int -> Int -> Int -> [[Rgb]] -> [[Rgb]]
applyFalseFloydSteinsberg w h row col rgb
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
        matrixWithError1 = setPixelDithering w h row (col + 1) 3 8 (errorR, errorG, errorB) newMatrix
        matrixWithError2 = setPixelDithering w h (row + 1) col 3 8 (errorR, errorG, errorB) matrixWithError1
        matrixWithError3 = setPixelDithering w h (row + 1) (col + 1) 2 8 (errorR, errorG, errorB) matrixWithError2
        newCol
          | col == w - 1 = 0
          | otherwise = col + 1
        newRow
          | col == w - 1 = row + 1
          | otherwise = row
     in applyFalseFloydSteinsberg w h newRow newCol matrixWithError3

falseFloydSteinberg :: Image -> Image
falseFloydSteinberg img = Image (format img) (width img) (height img) (applyFalseFloydSteinsberg (width img) (height img) 0 0 (content img)) (colors img)

execute :: FilePath -> FilePath -> IO ()
execute input output = do
  image <- loadImage input
  let newImage = falseFloydSteinberg (grayscale image)
  saveImage output newImage
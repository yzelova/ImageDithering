module Algorithms.OrderedDithering where

import Utils.Dithering (setPixel, setPixelDithering, setPixelOrderedDithering)
import Utils.FileOperations (loadImage, saveImage)
import Utils.Image (Image (..), Rgb (..), grayscale)

orderedDithering4Matrix :: [[Int]]
orderedDithering4Matrix =
  [ [0, 8, 2, 10],
    [12, 4, 14, 6],
    [3, 11, 1, 9],
    [15, 7, 13, 5]
  ]

orderedDithering8Matrix :: [[Int]]
orderedDithering8Matrix =
  [ [0, 32, 8, 40, 2, 34, 10, 42],
    [48, 16, 56, 24, 50, 18, 58, 26],
    [12, 44, 4, 36, 14, 46, 6, 38],
    [60, 28, 52, 20, 62, 30, 54, 22],
    [3, 35, 11, 43, 1, 33, 9, 41],
    [51, 19, 59, 27, 49, 17, 57, 25],
    [15, 47, 7, 39, 13, 45, 5, 37],
    [63, 31, 55, 23, 61, 29, 53, 21]
  ]

applyorderedDithering :: Int -> Int -> Int -> Int -> [[Int]] -> Int -> [[Rgb]] -> [[Rgb]]
applyorderedDithering w h row col m n rgb
  | row == h - 1 && col == w - 1 = rgb
  | otherwise =
    let newMatrix = setPixelOrderedDithering w h row col m n rgb
        newCol
          | col == w - 1 = 0
          | otherwise = col + 1
        newRow
          | col == w - 1 = row + 1
          | otherwise = row
     in applyorderedDithering w h newRow newCol m n newMatrix

orderedDithering4 :: Image -> Image
orderedDithering4 img =
  Image
    (format img)
    (width img)
    (height img)
    (applyorderedDithering (width img) (height img) 0 0 orderedDithering4Matrix 4 (content img))
    (colors img)

orderedDithering8 :: Image -> Image
orderedDithering8 img =
  Image
    (format img)
    (width img)
    (height img)
    (applyorderedDithering (width img) (height img) 0 0 orderedDithering8Matrix 8 (content img))
    (colors img)

executeBayer8 :: FilePath -> FilePath -> IO ()
executeBayer8 input output = do
  image <- loadImage input
  let newImage = orderedDithering8 (grayscale image)
  saveImage output newImage
executeBayer4 :: FilePath -> FilePath -> IO ()
executeBayer4 input output = do
  image <- loadImage input
  let newImage = orderedDithering4 (grayscale image)
  saveImage output newImage

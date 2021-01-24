module P3 where

import Data.ByteString as BS
import Image
import ParsingOperations

parsePlainTextRGB :: Int -> Int -> Int -> ByteString -> [[Rgb]]
parsePlainTextRGB width height colors bStr =
  let getRow :: Int -> ByteString -> ([Rgb], ByteString)
      getRow count bStr
        | count == width = ([], bStr)
        | BS.empty == trimWhiteSpace bStr = error "Invalid format"
        | otherwise =
          let (red, restR) = parseNumber bStr
              (green, restG) = parseNumber restR
              (blue, restB) = parseNumber restG
              rgb
                | fromIntegral red <= colors && fromIntegral green <= colors
                    && fromIntegral blue <= colors
                    && fromIntegral red >= 0
                    && fromIntegral green >= 0
                    && fromIntegral blue >= 0 =
                  Rgb (fromIntegral red) (fromIntegral green) (fromIntegral blue)
                | otherwise = error "Invalid color value"
              (restRow, restBStr) = getRow (count + 1) restB
           in (rgb : restRow, restBStr)
      getRows :: ByteString -> [[Rgb]]
      getRows bStr
        | BS.empty == trimWhiteSpace bStr = []
        | otherwise =
          let (row, rest) = getRow 0 bStr
           in row : getRows rest
      rows
        | Prelude.length (getRows bStr) /= height = error "Invalid format"
        | otherwise = getRows bStr
   in rows
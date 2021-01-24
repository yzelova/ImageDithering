module FormatsParsers.P2 where

import Data.ByteString as BS (ByteString, empty)
import Utils.Image (Rgb (..))
import Utils.ParsingOperations (parseNumber, trimWhiteSpace)

parsePlainTextGrayscale :: Int -> Int -> Int -> ByteString -> [[Rgb]]
parsePlainTextGrayscale width height colors bStr =
  let getRow :: Int -> ByteString -> ([Rgb], ByteString)
      getRow count bStr
        | count == width = ([], bStr)
        | BS.empty == trimWhiteSpace bStr = error "Invalid format"
        | otherwise =
          let (number, rest) = parseNumber bStr
              rgb
                | fromIntegral number <= colors && number >= 0 = Rgb (fromIntegral number) (fromIntegral number) (fromIntegral number)
                | otherwise = error "Invalid color value"
              (restRow, restBStr) = getRow (count + 1) rest
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

toStringP2 :: [[Rgb]] -> String
toStringP2 rgb =
  let rgbToString :: Rgb -> String
      rgbToString rgb
        | red rgb == green rgb && green rgb == blue rgb = show (red rgb) ++ "\n"
        | otherwise = error "Invalid format"
      toString :: [[Rgb]] -> [[String]]
      toString = Prelude.map $ Prelude.map rgbToString
   in Prelude.concat $ Prelude.concat (toString rgb)
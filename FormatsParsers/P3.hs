module FormatsParsers.P3 where

import Data.ByteString as BS ( ByteString, empty )
import Image ( Rgb(..) )
import ParsingOperations ( parseNumber, trimWhiteSpace )

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

toStringP3 :: [[Rgb]] -> String
toStringP3 rgb =
  let rgbToString :: Rgb -> String
      rgbToString rgb = show (red rgb) ++ " " ++ show (green rgb) ++ " " ++ show (blue rgb) ++ "\n"
      toString :: [[Rgb]] -> [[String]]
      toString = Prelude.map $ Prelude.map rgbToString
   in Prelude.concat $ Prelude.concat (toString rgb)
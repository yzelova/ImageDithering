module P6 where

import Data.ByteString as BS
import Data.Word
import Image

parseBinaryRGB :: Int -> Int -> Int -> ByteString -> [[Rgb]]
parseBinaryRGB width height colors bStr =
  let bytes = BS.unpack bStr
      getRow :: [Word8] -> [(Word8, Word8, Word8)]
      getRow bytes
        | Prelude.null bytes = []
        | otherwise =
          let red = Prelude.head bytes
              green = bytes !! 1
              blue = bytes !! 2
           in (red, green, blue) : getRow (Prelude.drop 3 bytes)
      getRows :: [Word8] -> [[(Word8, Word8, Word8)]]
      getRows bytes
        | Prelude.null bytes = []
        | Prelude.length bytes `mod` (3 * width) /= 0 = error "Invalid format"
        | otherwise = getRow (Prelude.take (3 * width) bytes) : getRows (Prelude.drop (3 * width) bytes)
      byteToRgb :: (Word8, Word8, Word8) -> Rgb
      byteToRgb (r, g, b)
        | fromIntegral r > colors || fromIntegral g > colors || fromIntegral b > colors = error "Invalid color value"
        | otherwise = Rgb r g b
      toRgb :: [[(Word8, Word8, Word8)]] -> [[Rgb]]
      toRgb = Prelude.map $ Prelude.map byteToRgb
   in toRgb $ getRows bytes

toByteStringP6 :: [[Rgb]] -> ByteString
toByteStringP6 rgb =
  let toWord8 :: Rgb -> [Word8]
      toWord8 rgb = [red rgb, green rgb, blue rgb]
      toWord8List :: [[Rgb]] -> [[[Word8]]]
      toWord8List = Prelude.map $ Prelude.map toWord8
   in BS.pack (Prelude.concat (Prelude.concat $ toWord8List rgb))
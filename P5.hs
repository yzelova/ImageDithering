module P5 where

import Data.ByteString as BS ( ByteString, pack, unpack )
import Data.Word ( Word8 )
import Data.Word8 ( Word8, _lf )
import Image ( Rgb(..) )

parseBinaryGrayscale :: Int -> Int -> Int -> ByteString -> [[Rgb]]
parseBinaryGrayscale width height colors bStr =
  let bytes = BS.unpack bStr
      getRows :: [Word8] -> [[Word8]]
      getRows bytes
        | Prelude.length bytes `mod` width /= 0 = error "Invalid format"
        | Prelude.null bytes = []
        | otherwise = Prelude.take width bytes : getRows (Prelude.drop width bytes)
      byteToRgb :: Word8 -> Rgb
      byteToRgb byte
        | byte < 0 = error "Invalid color value"
        | fromIntegral byte > colors = error "Invalid color value"
        | otherwise = Rgb byte byte byte
      toRgb :: [[Word8]] -> [[Rgb]]
      toRgb = Prelude.map $ Prelude.map byteToRgb
      rgb
        | Prelude.length (getRows bytes) /= height = error "Invalid format"
        | otherwise = toRgb $ getRows bytes
   in rgb

toByteStringP5 :: [[Rgb]] -> ByteString 
toByteStringP5 rgb =
  let toWord8 :: Rgb -> Word8
      toWord8 rgb
        | red rgb == green rgb && green rgb == blue rgb = red rgb
        | otherwise = error "Invalid format"
      toWord8List :: [[Rgb]] -> [[Word8]]
      toWord8List = Prelude.map $ Prelude.map toWord8
      addNewLine :: [[Word8]] -> [[Word8]]
      addNewLine matrix = Prelude.map (\line -> line ++ [_lf]) matrix
   in BS.pack (Prelude.concat $ addNewLine (toWord8List rgb))
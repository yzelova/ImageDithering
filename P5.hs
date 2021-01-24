module P5 where
import Data.ByteString as BS
import Image
import Data.Word

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
            | Prelude.length (getRows  bytes) /= height = error "Invalid format"
            | otherwise = toRgb $ getRows bytes
    in rgb
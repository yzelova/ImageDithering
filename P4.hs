module P4 where
import Data.ByteString as BS
import Image
import Data.Word

getNumOfBits :: Int -> Int
getNumOfBits num = count
  where
    helper :: Int -> Int -> Int -> Int
    helper cur num count
      | cur >= num = count
      | otherwise = helper (8 + cur) num (count + 1)
    count = helper 8 num 1


parseBinaryBlackWhite :: Int -> Int -> ByteString -> [[Rgb]]
parseBinaryBlackWhite width height bStr =
  let bytes = BS.unpack bStr
      bytesPerRow = getNumOfBits width
      getRowsBytes :: Int -> [Word8] -> [[Word8]]
      getRowsBytes bytesPerRow bytes 
        | Prelude.null bytes = []
        | Prelude.length bytes `mod` bytesPerRow /= 0 = error "Invalid format" 
        | otherwise  = Prelude.take bytesPerRow bytes : getRowsBytes bytesPerRow (Prelude.drop bytesPerRow bytes)
      rowsBytes = getRowsBytes bytesPerRow bytes
      getBits :: Word8 -> [Int]
      getBits byte =
        let 
          helper :: Int -> Word8 -> [Int]
          helper pow remaining
            | pow < 0 = []
            | remaining >= (2^pow) = 1 : helper (pow - 1) (remaining - 2^pow)
            | otherwise = 0 : helper (pow - 1) remaining 
        in helper 7 byte
      getRow :: [Word8] -> [Int]
      getRow rowBytes = Prelude.take width (Prelude.concatMap getBits rowBytes)
      getRows :: [[Word8]] -> [[Int]]
      getRows = Prelude.map getRow
      bitToRgb :: Int -> Rgb
      bitToRgb bit 
        | bit == 0 = Rgb 255 255 255
        | bit == 1 = Rgb 0 0 0
        | otherwise = error "Invalid value"
      toRgb :: [[Int]] -> [[Rgb]]
      toRgb = Prelude.map $ Prelude.map bitToRgb
      rgb 
        | Prelude.length (getRows rowsBytes) /= height = error "Invalid format" 
        | otherwise = toRgb $ getRows rowsBytes
  in rgb

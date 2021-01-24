module FormatsParsers.P4 where

import Data.ByteString as BS (ByteString, pack, unpack)
import Data.Word (Word8)
import Utils.Image (Rgb (..))

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
        | otherwise = Prelude.take bytesPerRow bytes : getRowsBytes bytesPerRow (Prelude.drop bytesPerRow bytes)
      rowsBytes = getRowsBytes bytesPerRow bytes
      getBits :: Word8 -> [Int]
      getBits byte =
        let helper :: Int -> Word8 -> [Int]
            helper pow remaining
              | pow < 0 = []
              | remaining >= (2 ^ pow) = 1 : helper (pow - 1) (remaining - 2 ^ pow)
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

toByteStringP4 :: Int -> [[Rgb]] -> ByteString
toByteStringP4 width rgb =
  let appendRow :: [Rgb] -> [Rgb]
      appendRow row =
        let neededCount = 8 * getNumOfBits width
            helper :: [Rgb] -> Int -> Int -> [Rgb]
            helper currentRow neededCount currentCount
              | neededCount == currentCount = currentRow
              | otherwise = helper (currentRow ++ [Rgb 255 255 255]) neededCount (currentCount + 1)
         in helper row neededCount (Prelude.length row)
      toWord8 :: Word8 -> [Rgb] -> Word8
      toWord8 currentNum rgb
        | Prelude.null rgb = currentNum
        | red (Prelude.head rgb) == 255 && blue (Prelude.head rgb) == 255 && green (Prelude.head rgb) == 255 = toWord8 (currentNum * 2) (Prelude.tail rgb)
        | red (Prelude.head rgb) == 0 && blue (Prelude.head rgb) == 0 && green (Prelude.head rgb) == 0 = toWord8 (currentNum * 2 + 1) (Prelude.tail rgb)
        | otherwise = error "Invalid format"
      getRow :: [Rgb] -> [Word8]
      getRow pixels =
        let appendedRow = appendRow pixels
            helper :: [Rgb] -> [Word8]
            helper rgb
              | Prelude.null rgb = []
              | otherwise = helper (Prelude.drop 8 rgb) ++ [toWord8 0 (Prelude.take 8 rgb)]
         in helper appendedRow
   in BS.pack (Prelude.concatMap getRow rgb)
module Utils.ParsingOperations where

import Data.ByteString as BS (ByteString, empty, head, tail)
import Data.Word8 as W8
  ( Word8,
    isSpace,
    _0,
    _1,
    _2,
    _3,
    _4,
    _5,
    _6,
    _7,
    _8,
    _9,
  )

trimWhiteSpace :: ByteString -> ByteString
trimWhiteSpace bStr
  | BS.empty == bStr = bStr
  | W8.isSpace $ BS.head bStr = trimWhiteSpace $ BS.tail bStr
  | otherwise = bStr

skipWhiteSpace :: ByteString -> ByteString
skipWhiteSpace bStr
  | BS.empty == bStr = bStr
  | W8.isSpace $ BS.head bStr = BS.tail bStr
  | otherwise = bStr

word8ToDigit :: Word8 -> Int
word8ToDigit w8
  | w8 == _1 = 1
  | w8 == _2 = 2
  | w8 == _3 = 3
  | w8 == _4 = 4
  | w8 == _5 = 5
  | w8 == _6 = 6
  | w8 == _7 = 7
  | w8 == _8 = 8
  | w8 == _9 = 9
  | w8 == _0 = 0
  | otherwise = error "Invalid argument"

parseNumber :: ByteString -> (Int, ByteString)
parseNumber bStr = (number, skipWhiteSpace rest)
  where
    trimmed = trimWhiteSpace bStr
    getNumber :: Int -> ByteString -> (Int, ByteString)
    getNumber result str
      | BS.empty == str = (result, str)
      | W8.isSpace $ BS.head str = (result, str)
      | otherwise = getNumber (result * 10 + word8ToDigit (BS.head str)) (BS.tail str)
    (number, rest) = getNumber 0 trimmed
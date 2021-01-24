module FileOperations where

import P1
import P2
import P3
import P4
import P5
import P6
import Data.ByteString as BS
import Data.Char
import Data.Text
import Data.Word8 as W8
import Image
import ParsingOperations
    ( parseNumber, trimWhiteSpace, skipWhiteSpace, word8ToDigit )

parseMagicNumber :: ByteString -> (Int, ByteString)
parseMagicNumber bStr = (magicNumber, skipWhiteSpace rest)
  where
    trimmed = trimWhiteSpace bStr
    withSkippedP
      | BS.empty == trimmed = error "Invalid format"
      | BS.head trimmed /= _P = error "Invalid format"
      | otherwise = BS.tail trimmed
    (magicNumber, rest)
      | BS.head withSkippedP < _1 || BS.head withSkippedP > _6 = error "Invalid magic number"
      | otherwise = (word8ToDigit $ BS.head withSkippedP, BS.tail withSkippedP)

-- reading files
loadImage :: String -> Prelude.IO [[Rgb]] 
loadImage path = do
  text <- BS.readFile path
  let (magicNumber, restMN) = parseMagicNumber text
      (width, restW) = parseNumber restMN
      (height, restH) = parseNumber restW
      (colors, restC)
        | magicNumber == 1 || magicNumber == 4 = (1, restH)
        | otherwise = parseNumber restH
      content
        | magicNumber == 1 = parsePlainTextBlackWhite width height restC
        | magicNumber == 2 = parsePlainTextGrayscale width height colors restC
        | magicNumber == 3 = parsePlainTextRGB width height colors restC
        | magicNumber == 4 = parseBinaryBlackWhite width height restC
        | magicNumber == 5 = parseBinaryGrayscale width height colors restC
        | magicNumber == 6 = parseBinaryRGB width height colors restC
  return content

module FileOperations where

import Data.ByteString as BS
    ( ByteString, empty, appendFile, head, readFile, tail )
import Data.Word8 as W8 ( _1, _6, _P )
import Image ( Image(..) )
import FormatsParsers.P1 ( parsePlainTextBlackWhite, toStringP1 )
import FormatsParsers.P2 ( parsePlainTextGrayscale, toStringP2 )
import FormatsParsers.P3 ( parsePlainTextRGB, toStringP3 )
import FormatsParsers.P4 ( parseBinaryBlackWhite, toByteStringP4 )
import FormatsParsers.P5 ( parseBinaryGrayscale, toByteStringP5 )
import FormatsParsers.P6 ( parseBinaryRGB, toByteStringP6 )
import ParsingOperations
  ( parseNumber,
    skipWhiteSpace,
    trimWhiteSpace,
    word8ToDigit,
  )

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

loadImage :: String -> Prelude.IO Image
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
        | otherwise = error "Invalid format"
  return (Image magicNumber (fromIntegral width) (fromIntegral height) content colors)

saveImage :: FilePath -> Image -> IO ()
saveImage path img = do
  let color
        | format img == 1 || format img == 4 = ""
        | otherwise = show (colors img) ++ "\n"
  _ <- Prelude.writeFile path "P"
  _ <- Prelude.appendFile path (show $ format img)
  _ <- Prelude.appendFile path "\n"
  _ <- Prelude.appendFile path (show $ width img)
  _ <- Prelude.appendFile path " "
  _ <- Prelude.appendFile path (show $ height img)
  _ <- Prelude.appendFile path "\n"
  _ <- Prelude.appendFile path color
  _ <-
    if format img == 1
      then Prelude.appendFile path (toStringP1 (content img))
      else
        if format img == 2
          then Prelude.appendFile path (toStringP2 (content img))
          else
            if format img == 3
              then Prelude.appendFile path (toStringP3 (content img))
              else
                if format img == 4
                  then BS.appendFile path (toByteStringP4 (width img) (content img))
                  else
                    if format img == 5
                      then BS.appendFile path (toByteStringP5 (content img))
                      else
                        if format img == 6
                          then BS.appendFile path (toByteStringP6 (content img))
                          else error "Invalid format"
  return ()
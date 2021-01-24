module P1 where
import Image
import Data.ByteString as BS
import ParsingOperations

parsePlainTextBlackWhite :: Int -> Int -> ByteString -> [[Rgb]]
parsePlainTextBlackWhite width height bStr =
    let getRow :: Int -> ByteString -> ([Rgb], ByteString)
        getRow count bStr 
            | count == width = ([], bStr)
            | BS.empty == trimWhiteSpace bStr = error "Invalid format"
            | otherwise = 
                let (number, rest) = parseNumber bStr
                    rgb 
                        | number == 0 = Rgb 255 255 255
                        | number == 1 = Rgb 0 0 0
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
            | Prelude.length  (getRows bStr) /= height = error "Invalid format"
            | otherwise = getRows bStr
    in rows

toStringP1 :: [[Rgb]] -> String 
toStringP1 rgb =
    let rgbToString :: Rgb -> String 
        rgbToString rgb 
            | red rgb == 255 && green rgb == 255 && blue rgb == 255 = "0\n"
            | red rgb == 0 && green rgb == 0 && blue rgb == 0 = "1\n"
            | otherwise = error "Invalid format"
        toString :: [[Rgb]] -> [[String]]
        toString = Prelude.map $ Prelude.map rgbToString
    in Prelude.concat $ Prelude.concat (toString rgb)
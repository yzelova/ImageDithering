module Image where

import Data.Word
import Data.Binary


data Rgb = Rgb
  { red :: Word8,
    green :: Word8,
    blue :: Word8
  } deriving (Read, Show)

instance Binary Rgb where 
  get = do 
          r <- get :: Get Word8
          g <- get :: Get Word8
          b <- get :: Get Word8
          return (Rgb r g b)
  put rgb = do 
        put (red rgb)
        put (green rgb)
        put (blue rgb)
                


data Image = Image 
  { format :: Int, 
    width::Word8,
    height::Word8,
    content::[[Rgb]]
  } deriving (Read, Show)

--conversion from-to formats

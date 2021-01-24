module Image where

import Data.Word
import Data.Binary
import Data.ByteString


data Rgb = Rgb
  { red :: Word8,
    green :: Word8,
    blue :: Word8
  } deriving (Read, Show)

data Image = Image 
  { format :: Int, 
    width::Int,
    height::Int,
    content::[[Rgb]],
    colors::Int
  } deriving (Read, Show)



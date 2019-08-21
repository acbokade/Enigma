{-| This is module for different conversions from one format to another
-}

module Conversions(
  fileToWord8,
  stringToWord8,
  word8ListToString,
  intToWord8,
  int32ToWord8,
  word8ToInt32,
  intToBits,
  boolToWord8
) 
where

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString.Char8 as C
import Data.Bits
import Data.Word
import Data.Char

{- | Get a sequence of Word8 for the given file.
   	 Input is file path.
 -}
fileToWord8 :: String -> IO [Word8]
fileToWord8 file = do
  fileContents <- B.readFile file -- Reads entire file in bytestring 
  return (B.unpack fileContents)  -- Converts bytestring to Word8

{- | Convert a string to Word8 format.
   | Input is string.
 -}
stringToWord8 :: String -> [Word8]
stringToWord8 str = map BI.c2w str

{- | Convert a sequence of Word8 to string.
 -}
word8ListToString :: [Word8] -> String
word8ListToString list = map BI.w2c list

{- | Convert Int to Word8
 -}
intToWord8 :: Int -> Word8
intToWord8 num = fromIntegral num

{- | Convert Int32 to Word8
 -}
int32ToWord8 :: Int -> [Word8]
int32ToWord8 x = map intToWord8 [(shiftR x (pos * 8)) .&. 255 | pos <- [0,1,2,3]]

{- | Convert Word8 to Int32
 -}
word8ToInt32 :: [Word8] -> Int
word8ToInt32 [] = 0
word8ToInt32 [x] = fromIntegral x
word8ToInt32 (x:xs) = (fromIntegral x) + shiftL (word8ToInt32 xs) 8

{- | Converts an integer to sequence of bits.
     testBit returns True if the nth bit of the argument is 1.
-}
intToBits :: Bits a => a -> Int -> [Bool]
intToBits x idx = map (testBit x) [0..idx - 1]

{-| Converts a sequence of bits to Word8
-}

boolToWord8 :: [Bool] -> Word8
boolToWord8 = foldl (\byte bit -> byte*2 + if bit then 1 else 0) 0

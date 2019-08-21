{-| Module for decoding the information hidden in the image using LSB steganography method
-}
module DecodeSteg(
    -- * Functions required for decoding information hidden in image 
  getWidth,
  getHeight,
  getRed,
  getGreen,
  getBlue,
  getTotalNumPixels,
  getTotalBits,
  getPixelsforMessage,
  getOptimumBits,
  readPixelBit,
  readBitOfImage,
  readByte,
  readFileName,
  readAllBytes,
  decodeImg
)
where

import Codec.Picture
import Data.Bits
import Data.Char 
import Data.Word
import qualified Data.ByteString.Lazy as B
import Conversions

-- | Obtain width of image
getWidth :: Image a -> Int 
getWidth (Image w _ _) = w

-- | Obtain height of image
getHeight :: Image a -> Int 
getHeight (Image _ h _) = h

-- | Obtain 8 bit red channel values from RGB8pixel of image
getRed   :: PixelRGB8 -> Pixel8
getRed   (PixelRGB8 r _ _) = r

-- | Obtain 8 bit green channel values from RGB8pixel of image
getGreen :: PixelRGB8 -> Pixel8
getGreen (PixelRGB8 _ g _) = g

-- | Obtain 8 bit blue channel values from RGB8pixel of image
getBlue  :: PixelRGB8 -> Pixel8
getBlue  (PixelRGB8 _ _ b) = b

-- | Get total number of pixels available to hide information in image
getTotalNumPixels :: Image PixelRGB8 -> Int
getTotalNumPixels img = getWidth img * getHeight img - 64 - 1

-- | Get total number of bits in which information can be hidden
getTotalBits :: Image PixelRGB8 -> Int -> Int
getTotalBits img bitsPerPixel = (getWidth img * getHeight img - 64 - 1) * bitsPerPixel * 3

-- | Get number of pixels in which information is hidden
getPixelsforMessage :: Int -> Int -> Int
getPixelsforMessage len bitsPerPixel = div (len * 8) (bitsPerPixel * 3)

-- | Get optimum number of last significant bits in pixel to hide the information in image
getOptimumBits :: Image PixelRGB8 -> Int -> Int
getOptimumBits img bytes = max (ceiling ((toRational (bytes * 8)) / (toRational ((getTotalNumPixels img) * 3)))) 1

-- | A delimiter used to separate file name and its data in encoded image
nullWord8 = intToWord8 0

-- | Test whether the pixel at given index is 0 or 1
readPixelBit :: Pixel8 -> Int -> Bool
readPixelBit px idx = testBit px idx

-- | Identifies which color channel of pixel is to be read and returns the bit at that color channel 
readBitOfImage :: Image PixelRGB8 -> Int -> Int -> Int -> Int -> Double -> Bool
readBitOfImage img bitsPerPixel byteIdx bitIdx offset period
  | color == 0 = readPixelBit (getRed   (pixelAt img px py)) lsbIdx
  | color == 1 = readPixelBit (getGreen (pixelAt img px py)) lsbIdx
  | color == 2 = readPixelBit (getBlue  (pixelAt img px py)) lsbIdx
  where 
    pos = floor (fromIntegral (byteIdx * 8 + bitIdx) * period)
    pixIdx = div pos (bitsPerPixel * 3) + offset 
    px = mod pixIdx (getWidth img)
    py = div pixIdx (getWidth img)
    color = div (mod pos (bitsPerPixel * 3)) bitsPerPixel
    lsbIdx = mod (mod pos (bitsPerPixel * 3)) bitsPerPixel

-- | Reads all the 8 bits of a byte (a character at given index of hidden information)
readByte :: Image PixelRGB8 -> Int -> Int -> Int -> Double -> Word8
readByte img idx lsb offset period = boolToWord8 [readBitOfImage img lsb idx i offset period | i <- reverse [0..7]]

-- | Reads the lsb bits of pixel from given starting index in which filename is stored till nullWord8 is encountered (delimiter) 
readFileName :: Image PixelRGB8 -> Int -> Int -> Int -> Double -> [Word8]
readFileName img start lsb offset period
  | byte == nullWord8 = [] 
  | otherwise         = [byte] ++ readFileName img (start + 1) lsb offset period
  where 
    byte = readByte img start lsb offset period

-- | Read all the bytes of image in which information is hidden 
readAllBytes :: Image PixelRGB8 -> Int -> Int -> Int -> Int -> Double -> [Word8]
readAllBytes img start lsb len offset period
  | len <= 0     = []
  | otherwise = (readByte img start lsb offset period):(readAllBytes img (start + 1) lsb (len - 1) offset period)

-- | Decrypt (read) bytes from file in which filename, filepath, information is hidden and writes it in a file
decodeImg :: Image PixelRGB8 -> String -> IO ()
decodeImg img o = do
  B.writeFile o (B.pack file)
  where
    len = fromIntegral (word8ToInt32 (readAllBytes img 0 1 4 0 (63 / (fromIntegral (getPixelsforMessage 4 1)))))
    bitsPerPixel = getOptimumBits img len
    lsb = bitsPerPixel
    name = readFileName img 0 lsb 64 (fromIntegral (getTotalBits img bitsPerPixel) / fromIntegral (len * 8))
    filePath = word8ListToString name
    file = readAllBytes img (length name + 1) lsb (len - (length name + 1)) 64 ((fromIntegral (getTotalBits img bitsPerPixel)) / (fromIntegral (len * 8)))

{-| Module for encoding (hiding) text file or image into another image using LSB Steganography.
-}
module EncodeSteg(
	getWidth,
	getHeight,
	getRed,
	getGreen,
	getBlue,
	getTotalNumPixels,
	getTotalBits,
	getPixelsforMessage,
	getMinBits,
	getChangedPixel, 
	encodeImage,
	writeBitToImage
)
where

import Codec.Picture
import Control.Monad.ST
import Data.Bits
import qualified Codec.Picture.Types as M
import Data.Word
import Conversions

{-| Returns width of image.
-}
getWidth :: Image a -> Int 
getWidth (Image w _ _) = w

{-| Returns height of image.
-}
getHeight :: Image a -> Int 
getHeight (Image _ h _) = h

{-| Returns red channel values from pixel
-}
getRed   :: PixelRGB8 -> Pixel8
getRed   (PixelRGB8 r _ _) = r

{-| Returns green channel values from pixel
-}
getGreen :: PixelRGB8 -> Pixel8
getGreen (PixelRGB8 _ g _) = g

{-| Returns blue channel values from pixel
-}
getBlue  :: PixelRGB8 -> Pixel8
getBlue  (PixelRGB8 _ _ b) = b

{-| Returns total number of pixels 
-}
getTotalNumPixels :: Image PixelRGB8 -> Int
getTotalNumPixels img = (getWidth img) * (getHeight img) - 64 - 1

{-| Returns total number of bits 
-}
getTotalBits :: Image PixelRGB8 -> Int -> Int
getTotalBits img bitsPerPixel = ((getWidth img) * (getHeight img) - 64 - 1) * bitsPerPixel * 3

{-| Returns total number of pixels to store message
-}
getPixelsforMessage :: Int -> Int -> Int
getPixelsforMessage len bitsPerPixel = div (len * 8) (bitsPerPixel * 3)

{-| Returns minimum bits to modify required per pixel color channel
-}
getMinBits :: Image PixelRGB8 -> Int -> Int
getMinBits img bytes = max (ceiling ((toRational (bytes * 8)) / (toRational ((getTotalNumPixels img) * 3)))) 1

{-| Modifies pixel 'pix' based on value of 'b'.
 	bit i is a value with the ith bit set and all other bits clear.
-} 
getChangedPixel :: Pixel8 -> Int -> Bool -> Pixel8
getChangedPixel pix x b
    | b     = pix .|. bit x
	| not b = pix .&. complement (bit x)

{-| Hides data 'message' in image.
	Returns new image similar to old image but with data hidden inside it.  
-} 
encodeImage :: Image PixelRGB8 -> Int -> [Word8] -> Either String (Image PixelRGB8)
encodeImage img bitsPerPixel message 
  | bitsPerPixel <= 8 = Right (runST $ do
      newimg <- M.unsafeThawImage img
      let 
        modifyBits i []     start b period = M.freezeImage newimg
        modifyBits i (x:xs) start b period = do
              writeBitToImage img newimg b i 0 (bits!!0) start period
              writeBitToImage img newimg b i 1 (bits!!1) start period
              writeBitToImage img newimg b i 2 (bits!!2) start period
              writeBitToImage img newimg b i 3 (bits!!3) start period
              writeBitToImage img newimg b i 4 (bits!!4) start period
              writeBitToImage img newimg b i 5 (bits!!5) start period
              writeBitToImage img newimg b i 6 (bits!!6) start period
              writeBitToImage img newimg b i 7 (bits!!7) start period
              modifyBits (i + 1) xs start b period
            where 
              bits = intToBits (x) 8
      modifyBits 0 (int32ToWord8 len) 0 1 (63 / (fromIntegral (getPixelsforMessage 4 1)))
      modifyBits 0 message 64 bitsPerPixel ((toRational (getTotalBits img bitsPerPixel)) / (toRational (len * 8)))
      )
  | otherwise = Left "Too long information to be encoded!!!"
  where 
    len = length message

{-| Writes data to image based on bits of message.  
-}
writeBitToImage orig img bitsPerPixel byteIdx bitIdx bitVal offset period
  | color == 0 = M.writePixel img px py (PixelRGB8 
    (getChangedPixel (getRed (pixelAt orig px py)) lsbIdx bitVal) 
    (getGreen (pixelAt orig px py)) 
    (getBlue (pixelAt orig px py)))
  | color == 1 = M.writePixel img px py (PixelRGB8 
    (getRed (pixelAt orig px py)) 
    (getChangedPixel (getGreen (pixelAt orig px py)) lsbIdx bitVal)
    (getBlue (pixelAt orig px py)))
  | color == 2 = M.writePixel img px py (PixelRGB8 
    (getRed (pixelAt orig px py)) 
    (getGreen (pixelAt orig px py)) 
    (getChangedPixel (getBlue (pixelAt orig px py)) lsbIdx bitVal))
  where
    pos = floor (fromIntegral (byteIdx * 8 + bitIdx) * period)
    p = (div pos (bitsPerPixel * 3)) + offset 
    px = mod p (getWidth orig)
    py = div p (getWidth orig)
    color = (div (mod pos (bitsPerPixel * 3)) bitsPerPixel)
    lsbIdx = (mod (mod pos (bitsPerPixel * 3)) bitsPerPixel)


module Testing where

import qualified Data.ByteString.Lazy as BS

isSameFile :: String -> String -> IO Bool
isSameFile firstFile secondFile = do
  firstFileContents <- BS.readFile firstFile
  secondFileContents <- BS.readFile secondFile
  return (firstFileContents == secondFileContents) 	  



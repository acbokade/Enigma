module Testing(
  isSameFile	
)
where

import qualified Data.ByteString.Lazy as BS

isSameFile :: String -> String -> IO Bool
isSameFile firstFile secondFile = do
  firstFileContents <- BS.readFile firstFile
  secondFileContents <- BS.readFile secondFile
  return (firstFileContents == secondFileContents) 	  

{---main = do
	result <- isSameFile "t1.txt" "t2.txt"
	if result then
		putStrLn "same files"
	else
		putStrLn "no same files"--}


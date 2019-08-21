import qualified Data.ByteString.Lazy as BS

main = do
	putStrLn "Enter first file name"
	a <- getLine
	putStrLn "Enter second file name"
	b <- getLine
	aContents <- BS.readFile a
	bContents <- BS.readFile b
	if(aContents == bContents) then
		putStrLn "The files are same"
	else
		putStrLn "The files are different"
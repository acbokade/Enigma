{-| Main Module for the project Enigma
-}
module Main(
	main,
	encimg,
	decimg,
	key,
	enctxt,
	dectxt
)
where

import EncodeSteg
import DecodeSteg
import Conversions
import RsaGenKeys 
import RsaEncrypt 
import RsaDecrypt 
import System.Environment
import Codec.Picture 
import Testing

{-| Hiding information(text/image) into an image(Steganography).
	infile1 represents original image.
	infile2 represents information to hide.
	outfile represents output image file.
-}
encimg infile1 infile2 outfile = do
	imageLoad <- readImage infile1
	hidedata <-  fileToWord8 infile2
	case imageLoad of
		Left error  -> putStrLn error
		Right image -> do
			let
				conv = (convertRGB8 image)
				len = (length infile2) + 1 + (length hidedata)
				bitsPerPixel = getMinBits conv len
				message = (stringToWord8 infile2) ++ [intToWord8 0] ++ hidedata
				finalimg = (encodeImage conv bitsPerPixel message)
			case finalimg of 
			  	Left errorStr   -> putStrLn errorStr
			  	Right encrypted -> do 
			  		savePngImage outfile $ ImageRGB8 encrypted
			putStrLn("Done")

{-|	Extracts hidden data from an image.
	infile represents original image.
	outfile represents output hidden data.
-}
decimg infile outfile = do
	imageLoad <- readImage infile
	case imageLoad of
		Left error  -> putStrLn error
		Right image -> do
			let 
	  			conv = (convertRGB8 image)
			decodeImg conv outfile

{-|Generates key with given username
-}
key usrname = do
	rsaGenKeys usrname

{-|	Encrpyts a text file using RSA and hide into image
	infile1 represents original image.
	infile2 represents text to hide.
	outfile represents image with information hidden.
-}
enctxt infile1 infile2 name outfile = do
	rsaEncrypt infile2 name "enctemp.txt"
	encimg infile1 "enctemp.txt" outfile

{-|	Decrypts a text file using RSA.
	infile represents original image.
	name represents username used to create key.
	outfile represents encypted text hidden.
-}
dectxt infile name outfile = do 
	decimg infile "dectemp.txt"
	rsaDecrypt "dectemp.txt" name outfile

{-| main function
-}
main = do
	args <- getArgs 
	let
		option = (args!!0)
	case option of 
		"-enc" -> encimg (args!!1) (args!!2) (args!!3)
		"-dec" -> decimg (args!!1) (args!!2)
		"-k" -> key (args!!1)
		"-et" -> enctxt (args!!1) (args!!2) (args!!3) (args!!4)
		"-dt" -> dectxt (args!!1) (args!!2) (args!!3) 
		_   -> putStr "Invalid option"
	putStrLn("Hurray")
	{--result <- isSameFile "t1.txt" "t2.txt"
	if result then
		putStrLn "same files"
	else
		putStrLn "no same files"--}
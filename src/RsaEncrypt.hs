{-| Module for RSA Encryption
-}
module RsaEncrypt 
    (
        rsaEncrypt
    ) where

import Math.NumberTheory.Primes.Testing
import System.Environment
import System.IO
import System.IO.Error
import Data.List
import Data.Char
import Data.List.Split
import ModularArithmetics


{-|Encode a string to numbers by using foldl.Characters are separated by multiplying them with 1000.
-}
encode::[String]->[Int]
encode x=map (foldl (\a b->1000*a+b) 0) blcks 
    where blcks = map (map ord) x


{-|Encrypt the numbers using the exponent in public key as per RSA.
-}
encrypt::Integer->Integer->Integer->Integer
encrypt e n m = powerMod m e n

{-|Encrypt the input file for the user to the output file.
-}
rsaEncrypt::String->String->String->IO ()
rsaEncrypt input user output = do
    encfile<-(readFile input)   
    publicKey<-(readFile (user++".pub"))
    let blocks=chunksOf 6 encfile
        encoded= encode blocks
        starray=splitOn "/" publicKey
        encExponent=read(starray!!0)::Integer
        modulus=read(starray!!1)::Integer
        encBlocks=map (encrypt encExponent modulus) (map toInteger encoded)
        outputFile=foldl (\a b-> a++(show b)++"/") "" encBlocks
    writeFile ((output)) outputFile


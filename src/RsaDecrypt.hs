{-| Module for RSA Decryption
-}
module RsaDecrypt 
    (
        rsaDecrypt
    ) where

import Math.NumberTheory.Primes.Testing
import System.Environment
import System.IO
import System.IO.Error
import Data.List
import Data.Char
import Data.List.Split
import ModularArithmetics

{-|Decode the whole number into smaller numbers for preparation of converting back to the file.
-}
decode::Int->[Int]
decode x = if x==0 
            then []
            else (decode (x `quot` 1000))++[(x `mod` 1000)]

{-|Decrypt the number by using the exponent from private key as per RSA.
-}
decrypt::Integer->Integer->Integer->Integer
decrypt a b c=powerMod c a b

{-|Removing the last element of a list.
-}
pop::[String]->[String]
pop s
            | tail s == [] = []
            | otherwise = head(s):(pop (tail s))

{-|Decrypt the input file for the user to output file.First Decrypt, then decode and then conversion takes place.
-}
rsaDecrypt::String->String->String->IO ()
rsaDecrypt input user output = do
    inputFile<-(readFile (input))
    privateKey<-(readFile ((user)++".pvt"))
    let starray=splitOn "/" privateKey
        decryptKey=read (starray!!0)::Integer
        modulus=read (starray!!1)::Integer
        blocks=splitOn "/" inputFile 
        intBlocks=map read (pop blocks)
        decBlocks= map (decrypt decryptKey modulus) intBlocks
        decIntBlocks=map fromInteger decBlocks
        decoded=map decode decIntBlocks
        decodedBlock=map (foldl (\a b->a++((chr b):[])) "") decoded
        outputFile=foldl (\a b-> a++b) "" decodedBlock
    writeFile (output) outputFile


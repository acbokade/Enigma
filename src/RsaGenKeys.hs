{-|Module for creating key for RSA encryption
-}
module RsaGenKeys 
    (
        rsaGenKeys
    ) where

import System.IO
import System.Environment
import System.Random
import System.IO.Unsafe
import Data.Maybe
import ModularArithmetics
import Math.NumberTheory.Primes.Testing


{-|Calculates the part of the keys for the encryption using the input phi and generating suitable exponents.
-}
genKeysExp::Integer->(Integer, Integer)
genKeysExp x= let e=gcoprime x in (e,inverseMod e x)

{-|Calculates HCF or GCD of two numbers
-}
hcf::Integer->Integer->Integer
hcf x y
                |(x/=0 && y == 0) = abs x
                |otherwise = hcf b (mod a b)
                where a = abs x
                      b = abs y

{-|Check whether two numbers are coprime or not using hcf function
-}
isCoprime::Integer->Integer->Bool
isCoprime a b
                |hcf a b == 1 = True
                |otherwise = False

{-|Gets a random coprime number less than the arguement
-}
gcoprime::Integer->Integer
gcoprime x=if isCoprime x y
              then y
              else gcoprime x
              where y=unsafePerformIO (randomRIO (0, x-2))

{-|Generates a random prime number using RandomRIO for faster generation and tests using inbuilt prime  
-}
genPrime::IO Integer
genPrime = do 
    x<-randomRIO (0,993829173981749872118947218947912873982174821739827148721479821749827184729184798271312984021651029470826105281603972160836921630927106386120937091793829)
    if (isPrime x) 
        then return x
        else genPrime

{-|Function for generating the public and private keys according to the RSA algorithm
-}
rsaGenKeys::String->IO ()
rsaGenKeys user = do
    prime_p<-genPrime
    prime_q<-genPrime
    let name=(user)
        pub_name=name++".pub"
        priv_name=name++".pvt"
        modulus=prime_p*prime_q
        phi=(prime_p-1)*(prime_q-1)
        (e, d)=genKeysExp phi
        pub_key=show e++"/"++(show modulus)
        priv_key=show d++"/"++(show modulus)
    writeFile pub_name pub_key
    writeFile priv_name priv_key
    return ()


{-| Module for some mathematical functions
-}
module ModularArithmetics 
    (
        inverseMod,
        euclid,
        powerMod
    ) where


{-|Calculates modular inverse of a number for a given mod parameter.
-}
inverseMod :: Integer -> Integer -> Integer
inverseMod e phi =
  (x + phi) `mod` phi
  where
    (x, y) = euclid e phi

{- | Extended Euclidean theorem for the co-efficients.It gives the co-efficients for producing the gcd.
 -}
euclid :: Integer -> Integer -> (Integer, Integer)
euclid 0 _ = (0,1)
euclid _ 0 = (1,0)
euclid e n = (t, s-q*t)
    where
      (q, r) = quotRem e n
      (s, t) = euclid n r

{- | Finds base raised to the exponent mod modul for calculations.It uses fast modular exponentiation.
 -}
powerMod :: (Integral a, Integral b) => a -> b -> a -> a
powerMod base exponent modul 
    | exponent == 0 = 1
    | (odd exponent) = ( base * (powerMod (mbsq) (exponent `div` 2) modul)) `mod` modul 
    | otherwise = (powerMod (mbsq) (exponent `div` 2) modul)
    where mbsq = (base*base) `mod` modul


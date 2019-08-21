module ModularArithmetics 
    (
        inverseMod,
        euclid,
        powerMod
    ) where


{-|Calculates modular inverse
-}
inverseMod :: Integer -> Integer -> Integer
inverseMod e phi =
  (x + phi) `mod` phi
  where
    (x, y) = euclid e phi

{- | Extended Euclidean theorem for the co-efficients.
 -}
euclid :: Integer -> Integer -> (Integer, Integer)
euclid 0 _ = (0,1)
euclid _ 0 = (1,0)
euclid e n = (t, s-q*t)
    where
      (q, r) = quotRem e n
      (s, t) = euclid n r

{- | Finds base^exponent mod modul for calculations.
 -}
powerMod :: (Integral a, Integral b) => a -> b -> a -> a
powerMod base exponent modul 
    | exponent == 0 = 1
    | (odd exponent) = ( base * (powerMod (mbsq) (exponent `div` 2) modul)) `mod` modul 
    | otherwise = (powerMod (mbsq) (exponent `div` 2) modul)
    where mbsq = (base*base) `mod` modul


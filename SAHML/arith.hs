{-
Andrew Ribeiro
January 2018
arith.hs
Elementary arithmetic functions. 
-}
module SAHML.Arith (intSqrt, dt, pfs, spfs,eSieve,isPrime)
where

import SAHML.Helpers

-- Integer Square Root
intSqrt :: (Integral a) => a -> Integer
intSqrt x =  round (sqrt (fromIntegral x))

-- Divisor Tuples
dt :: Integer -> [(Integer, Integer)]
dt 0 = [(0,1)]
dt x = [ (quot x i,i) 
        | i <- [k,k-1..1], mod x i == 0]
        where k = intSqrt(x)


-- Prime Factors. Can factor numbers up to the trillions easily.
pfs :: Integer -> [Integer]
pfs x
    | elem 1 z = [x]
    | otherwise = pfs (fst z) ++ pfs (snd z)
    where z = head (dt x)

-- Is Prime: True if x is prime, False if x is not prime. 
isPrime :: Integer -> Bool
isPrime x = elem 1 (head (dt x) )

-- Sorted Prime Factors of x
spfs x = quicksort(pfs x)

eSieve [] _ = []
eSieve n s = s:(eSieve k (head k))
                  where k = [ a | a <- n, mod a s /= 0 ] 
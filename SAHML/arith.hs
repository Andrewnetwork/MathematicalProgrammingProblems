{-
Andrew Ribeiro
January 2018
arith.hs
Elementary arithmetic functions. 
-}
module SAHML.Arith (intSqrt, dt, pfs, quicksort, spfs)
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

-- Source: http://learnyouahaskell.com/recursion#hello-recursion
quicksort :: (Ord a) => [a] -> [a]  
quicksort [] = []  
quicksort (x:xs) =   
    let smallerSorted = quicksort [a | a <- xs, a <= x]  
        biggerSorted = quicksort [a | a <- xs, a > x]  
    in  smallerSorted ++ [x] ++ biggerSorted 

-- Sorted Prime Factors of x
spfs x = quicksort(pfs x)
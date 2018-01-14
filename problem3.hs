{-
Problem 3: Largest prime factor

The prime factors of 13195 are 5, 7, 13 and 29.

What is the largest prime factor of the number 600851475143 ?
-}

-- Integer Square Root
intSqrt :: (Integral a) => a -> Integer
intSqrt x =  round (sqrt (fromIntegral x))

-- Divisor Tuples
dt :: Integer -> [(Integer, Integer)]
dt x = [ (quot x i,i) 
        | i <- [k,k-1..1], mod x i == 0]
        where k = intSqrt(x)

-- Prime Factors 
pfs :: Integer -> [Integer]
pfs x
    | elem 1 z = [x]
    | otherwise = pfs (fst z) ++ pfs (snd z)
    where z = head (dt x)
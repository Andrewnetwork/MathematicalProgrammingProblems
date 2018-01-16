{-
Problem 5: Smallest multiple
2520 is the smallest number that can be divided by each of the numbers from 1 to 10 
without any remainder.

What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?
-}

import SAHML.Arith
import SAHML.Helpers

-- Get largest grouping of prime factors over all prime factorizations. 
p5 = map pfs [2,3..20]
-- 2*2*2*2* 3*3* 5* 7* 11 *13 *17 *19 = 232792560

unique ls = quicksort ( setisfy (concat ls) )
-- unique (map pfs [2,3..20])

count :: Integer -> [Integer] -> Integer
count i ls = toInteger (length (filter (== i) ls))
-- count 3 [4,5,6,3,4,3]

multTuples mults = [(a,(maximum (map (count a) pfls)))| a <- unique pfls] 
                     where pfls = (map pfs mults)
-- multTuples [2,3..20]

smallestMultiple mults = product (map (\(x,y)->x^y) (multTuples mults))
-- smallestMultiple [2,3..20]

p5Answer = smallestMultiple [2,3..20]

 
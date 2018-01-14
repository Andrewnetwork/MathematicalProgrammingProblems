{-
Problem 5: Smallest multiple
2520 is the smallest number that can be divided by each of the numbers from 1 to 10 
without any remainder.

What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?
-}

-- Get largest grouping of prime factors over all prime factorizations. 
p4 = map pfs [2,3..20]

-- 2*2*2*2*3*3*5*7*11*13*17*19
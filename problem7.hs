{-
Problem 7: 10001'st prime

By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.

What is the 10,001st prime number?
-}
eSieve [] _ = []
eSieve n s = s:(eSieve k (head k))
                  where k = [ a | a <- n, mod a s /= 0 ] 
-- eSieve [2,3..100] 2 
-- Answer: last (take 10001 (eSieve [2,3..] 2))              
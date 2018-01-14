{-
Problem 4: Largest palindrome product

A palindromic number reads the same both ways. 
The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.

Find the largest palindrome made from the product of two 3-digit numbers.
-}
p4 = [pal | pal <- [ show (z*y) | z <- [100,101..999], y <- [100,101..999]],pal == reverse pal]
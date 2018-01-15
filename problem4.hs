{-
Problem 4: Largest palindrome product

A palindromic number reads the same both ways. 
The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.

Find the largest palindrome made from the product of two 3-digit numbers.
-}

import SAHML.Arith
import SAHML.Helpers
-- A brute force way of searching through all palindromes made from the product of two three digit numbers. 
-- We can look through this list for the largest palindrome. 
p4 = [read pal::Int | pal <- [ show (z*y) | z <- [100,101..999], y <- [100,101..999]],pal == reverse pal]

-- r1,r2 are two lists of natural numbers
-- pal : the palindromes of all combinations of r1 * r2. 
pal r1 r2 = [read pal::Int | pal <- [ show (z*y) | z <- r1, y <- r2],pal == reverse pal]

-- Pal of type 1: The digits or fundamental units of the palindrome construction. Ex: [0,1,..9].
-- Pal of type 2: p1::1 ++ p2::1 where p1 == p2. Ex: [00,11,22,33,44,..99]
-- Pal of type 3: p1::1 ++ p2::1 ++ p1::1. Ex: 101, 101. 
-- Pal of type 4: p1::1 ++ p2::2 ++ p1::1. Ex:0330,1001. 
-- Pal of type 5: p1::1 ++ p2::3 ++ p1::1. Ex: 30303.
-- Pal of type 6: p1::1 ++ p2::4 ++ p1::1 . Ex: 11 22 11, 9 0660 9 
twoDigitNumStrs = let digits = ['0','1'..'9'] in [ l:m:[] | l <- digits, m <- digits] 

-- CP Construct Palindrome 
-- palindromeLength
-- lexemes = List of strings. 
cp 1 lexemes = lexemes

cp 2 lexemes = [l++r| l <- pal1, r <- pal1, l == r]
               where pal1 = (cp 1 lexemes )
               
cp x lexemes = [ l++m++r | l <- pal1, m <- palM2, r <- pal1, r == l] 
               where pal1 = (cp 1 lexemes)
                     palM2 = (cp (x-2) lexemes)

constructPal x = cp x (map show [0,1..9]) 

--cp 2 (map show [0,1..9])
--cp 4 (map etle ['a','b'..'z'])
topDt x = head (dt x)

p4' = [ z | z <- map topDt cp6, fst z > 100 && fst z < 999 && snd z > 100 && snd z < 999]
     where cp6 = slil (constructPal 6)

p4Ans = let i = last p4' in fst i * snd i
    
nPal x = length (constructPal x)
--  map nPal [1,2..10]
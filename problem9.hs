{-
Problem 9: Special Pythagorean triplet
A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,

a^2 + b^2 = c^2
For example, 3^2 + 4^2 = 9 + 16 = 25 = 52.

There exists exactly one Pythagorean triplet for which a + b + c = 1000.
Find the product abc.

a^2 + b^2 = c^2
a^2 = c^2 - b^2
b^2 = c^2 - a^2
a + b + c = 1000
(a + b) - c = 
-}

import SAHML.Helpers
import Data.List

sumDiff ls = (ls !! 0) + (ls !! 1) - (ls !! 2)
genPythTrip upperLim = [ [a,b,c] | a <- [2,3..upperLim],b <- [2,3..upperLim], c <- [2,3..upperLim], a^2 + b^2 == c ^2]

isPythTrip [a,b,c] = (a^2 + b^2 == c ^2)

sortFun (a1,b1) (a2,b2)
    | b1 < b2 = LT
    | b1 > b2 = GT
    | otherwise = EQ

-- sortBy sortFun (zip (genPythTrip 200) (map sum (genPythTrip 200)))
-- From the above we find a tripple which can be scaled to our answer: [40,75,85],200 -- 1000/200 = 5
p9Ans = product ( map (*5) [40,75,85] )

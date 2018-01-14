{-
Problem 1: Multiples of 3 and 5

If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. 
The sum of these multiples is 23.

Find the sum of all the multiples of 3 or 5 below 1000.
-}

-- strt:         start of range [strt,stop]
-- stop:         stop of range [strt,stop]
-- multInRange : multiples of mult in [strt,stop]
multInRange strt stop mult = floor((stop-strt)/mult)

-- sumNat n = 1 + 2 + 3 + ... + n
sumNat n = (n*(n+1))/2

-- strt:     start of range [strt,stop]
-- stop:     stop of range [strt,stop]
-- sumMult : sum of multiples of mult in [strt,stop]
sumMult strt stop mult = mult * (sumNat (fromInteger (multInRange strt stop mult)))
--(sumMult 0 999 3) + (sumMult 0 999 5 ) - (sumMult 0 999 (3*5) )

-- mls:   multiples list. 
-- lower: start of range [lower,upper]
-- upper: end of range [lower,upper]
-- multiples: the multiples of all elements in mls in [lower,upper]
multiples mls lower upper = [z | z <- [lower+1,lower+2..upper], elem 0 (map (mod z) mls) ]
-- sum( multiples [3,5] 1 999 )
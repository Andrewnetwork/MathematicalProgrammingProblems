
divide n d = n/d

ndivide x = map (divide x) [1,2..x]

filterFn x = map wholeReturn (ndivide x)

wholeReturn x
    | isInteger x = x
    | otherwise   = 0

wholeReturn' x
    | isInteger x = Just x
    | otherwise = Nothing 

isEven x
    | (x `mod` 2) == 0 = True
    | otherwise        = False  

-- 20: 1,  2,4,5,10   ,20
-- 20: 2 * 2 * 5 

isInteger :: Float -> Bool
isInteger x = fromInteger (round x) == x

{-
Problem 1: Multiples of 3 and 5

If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. 
The sum of these multiples is 23.

Find the sum of all the multiples of 3 or 5 below 1000.
-}

multiples mls lower upper = [z | z <- [lower,lower+1..upper], elem 0 (map (mod z) mls) ]

-- sum( multiples [3,5] 1 999 )

divisors x = [d | d <- [] ]


tupToStr :: (Show a) => (a,a) -> [Char]
tupToStr tup = show (fst tup) ++  show (snd tup)

constructPal 2 = map foldStrTup (zip (constructPal 1) (constructPal 1))
-- L M R. Where L=R and M,L,R in [0,1..9]. 

foldStrTup tup = (fst tup) ++ (snd tup)
-- Construct Palindrome palindromeLength
-- He we build up specific solutions in order to gain intuition to the general one. 
constructPal 1 = map show [0,1..9]

constructPal 2 = [l++r| l <- pal1, r <- pal1, l == r]
                 where pal1 = (constructPal 1)

constructPal 3 = [ l++m++r | l <- pal1, m <- pal1, r <- pal1, r == l] 
                 where pal1 = (constructPal 1)

constructPal 4 = [ l++m++r | l <- pal1, m <- pal2, r <- pal1, r == l] 
                 where pal1 = (constructPal 1)
                       pal2 = (constructPal 2)

constructPal 5 = [ l++m++r | l <- pal1, m <- pal3, r <- pal1, r == l] 
                 where pal1 = (constructPal 1)
                       pal3 = (constructPal 3)

constructPal 6 = [ l++m++r | l <- pal1, m <- pal4, r <- pal1, r == l] 
                 where pal1 = (constructPal 1)
                       pal4 = (constructPal 4)

constructPal x = [ l++m++r | l <- pal1, m <- palM2, r <- pal1, r == l] 
                where pal1 = (constructPal 1)
                      palM2 = (constructPal (x-2))

-- He we build up specific solutions in order to gain intuition to the general one. 
constructPal 1 = map show [0,1..9]

constructPal 2 = [l++r| l <- pal1, r <- pal1, l == r]
                 where pal1 = (constructPal 1)

constructPal x = [ l++m++r | l <- pal1, m <- palM2, r <- pal1, r == l] 
                 where pal1 = (constructPal 1)
                       palM2 = (constructPal (x-2))
unique [] = []
unique (x:xs) = [ (x,a) | a <- xs ]

lso [] = []
lso (x:xs) = [ b:r | a <- xs, b <- x, r <- lso xs,  not (elem b a) ]
-- elem True (map (elem b) a)

skip _ [] = []
skip i ls
    | s == [] = f
    | otherwise = f ++ [0] ++ tail rest
    where (f,s) = (splitAt i ls)
          rest = skip i s
-- skip 2 [2,3,4,5,6]
-- skip 3 (skip 2 [2,3,4,5,6,7,8] )
-- skip 2 [2,3..20]
-- skip 3 (skip 2 [2,3..20] )
-- skip 3 (tail (skip 2 [2,3..20] ) )
-- skip 4 (tail (skip 3 (tail (skip 2 [2,3..100] ) ) ) )
-- skip 5 (skip 4 (tail (skip 3 (tail (skip 2 [2,3..100] ) ) ) ) )
eSieve [] _ = []
eSieve n s = s:(eSieve k (head k))
                  where k = [ a | a <- n, mod a s /= 0 ] 
-- (map read (words grid))::[Int]

transv start end ls
    | start >= end = []
    | otherwise = [map (!! start) numGrid] ++ (transpose start+1 end ls)
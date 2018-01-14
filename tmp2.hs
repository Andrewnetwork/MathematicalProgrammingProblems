
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
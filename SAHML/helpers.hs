module SAHML.Helpers (etle,slil,setisfy,quicksort,windows,predPartition,ltPartition)
where 

import Data.List

-- Element to List element
etle :: a -> [a]
etle c = [c]

-- String List to Integer List
slil :: [String] -> [Integer]
slil x = map read x

-- setisfy: takes a list and returns a set
setisfy [] = []
setisfy (x:xs)
    | elem x xs = setisfy xs
    | otherwise = x : setisfy xs

-- Source: http://learnyouahaskell.com/recursion#hello-recursion
quicksort :: (Ord a) => [a] -> [a]  
quicksort [] = []  
quicksort (x:xs) =   
    let smallerSorted = quicksort [a | a <- xs, a <= x]  
        biggerSorted = quicksort [a | a <- xs, a > x]  
    in  smallerSorted ++ [x] ++ biggerSorted 

windows _ [] = []
windows n (x:xs)
    | (length xs) > (n-2) = [x:(take (n-1) xs)]++(windows n xs)
    | otherwise = []

-- Produces sublists wherever the predicated holds true on a list. 
predPartition _ [] = []
predPartition p xs 
    | t == [] = [h]
    | otherwise = (h++[head t]) : predPartition p (tail t) 
    where (h,t) = break p xs
-- predPartition (\x -> mod x 10 == 0) [1,2..100]

ltPartition _ _ [] = []
ltPartition inc s xs 
    | t == [] = [h]
    | otherwise = h : ltPartition inc (s+inc) (t) 
    where (h,t) = break (\x -> x >= s) xs

-- ltPartition 10 10 [1,2..100]
-- map sum (ltPartition 10 10 [1,2..100])
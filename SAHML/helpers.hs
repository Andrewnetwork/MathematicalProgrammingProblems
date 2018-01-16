module SAHML.Helpers (etle,slil,setisfy)
where 

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
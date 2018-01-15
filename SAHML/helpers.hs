module SAHML.Helpers (etle,slil)
where 

-- Element to List element
etle :: a -> [a]
etle c = [c]

-- String List to Integer List
slil :: [String] -> [Integer]
slil x = map read x

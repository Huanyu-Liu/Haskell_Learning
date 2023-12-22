module Composition where

import Prelude hiding((.), and)

sumEvenSquares :: [Int] -> Int
sumEvenSquares xs = sum $ map (\ x -> x * x) $ filter even xs

sumEvenSquares' :: [Int] -> Int
sumEvenSquares' = sum . (map (\ x -> x * x)) . (filter even)

(.) :: (b -> c) -> (a -> b) -> (a -> c)
f . g = \ x -> f (g x)

and :: [Bool] -> Bool
and = all id

listOfFunctions :: [Int -> Int]
listOfFunctions = [(+1), (-2), (*3)]

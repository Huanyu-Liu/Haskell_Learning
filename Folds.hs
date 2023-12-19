module Folds where

import Prelude hiding (and, elem, map, filter, zip)

elem :: Eq a => a -> [a] -> Bool
elem _ [] = False
elem x (y : ys) = x == y || elem x ys

fun :: (a -> r -> r) -> r -> [a] -> r
fun cons nil [] = nil
fun cons nil (x : xs) = cons x (fun cons nil xs)

elem' :: Eq a => a -> [a] -> Bool
elem' y = fun (\ x r -> x == y || r) False

map' :: (a -> b) -> [a] -> [b]
map' f = fun (\ x r -> f x : r) []

and' :: [Bool] -> Bool
and' = fun (&&) True

or' :: [Bool] -> Bool
or' = foldr (||) False

filter' :: (a -> Bool) -> [a] -> [a]
filter' f = foldr (\ x r -> case f x of
                                True -> x : r
                                False -> r) []

filter :: (a -> Bool) -> [a] -> [a]
filter f [] = []
filter f (x : xs)
    | f x = x : filter f xs
    | otherwise = filter f xs

any' :: (a -> Bool) -> [a] -> Bool
any' f = foldr (\ x r -> f x || r) False


zip' :: [a] -> [b] -> [(a, b)]
zip' (x : xs) = foldr (\ y r -> (x, y) : r) []
zip' _ _ = []

zip :: [a] -> [b] -> [(a, b)]
zip (x : xs) (y : ys) = (x, y) : zip xs ys
zip _ _ = []

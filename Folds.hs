{-# LANGUAGE BangPatterns #-}

module Folds where

import Prelude hiding (and, elem, map, filter, zip, foldr, flip)

elem :: Eq a => a -> [a] -> Bool
elem _ [] = False
elem x (y : ys) = x == y || elem x ys

elem' :: Eq a => a -> [a] -> Bool
elem' y = foldr (\ x r -> x == y || r) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\ x r -> f x : r) []

and' :: [Bool] -> Bool
and' = foldr (&&) True

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
zip' [] = const []

zip :: [a] -> [b] -> [(a, b)]
zip (x : xs) (y : ys) = (x, y) : zip xs ys
zip _ _ = []

foldr :: (a -> r -> r) -> r -> [a] -> r
foldr cons nil [] = nil
foldr cons nil (x : xs) = cons x (foldr cons nil xs)

-- foldr cons nil [x, y, z]
-- = foldr cons nil (x : y : z : [])
-- = x `cons` (foldr cons nil (y : z : []))
-- = x `cons` (y `cons` (foldr cons nil (z : [])))
-- = x `cons` (y `cons` (z `cons` (foldr cons nil [])))
-- = x `cons` (y `cons` (z `cons` nil))

-- foldl' upd ini [x, y, z]
-- = foldl' upd init (x : (y : (z : [])))
-- = go ini (x : (y : (z : [])))
-- = go (ini `upd` x) (y : (z : []))
-- = go ((ini `upd` x) `upd` y) (z : [])
-- = go (((ini `upd` x) `upd` y) `upd` z) []
-- = (((ini `upd` x) `upd` y) `upd` z)

foldl' :: (r -> a -> r) -> r -> [a] -> r
foldl' upd ini = go ini
    where
        go !acc [] = acc
        go !acc (x : xs) = go (upd acc x) xs

reverse' :: [a] -> [a]
reverse' = foldl' (flip (:)) []

flip :: (a -> b -> c) -> b -> a -> c
flip f a b = f b a

sum' :: Num a => [a] -> a
sum' = foldl' (+) 0

length' :: [a] -> Int
length' = foldl' (\ acc _ -> acc + 1) 0

maximum' :: Ord a => [a] -> Maybe a
maximum' = foldl' (\ acc x -> case acc of
                                Nothing -> Just x
                                Just z -> Just (max z x)) Nothing

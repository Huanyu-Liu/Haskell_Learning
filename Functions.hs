module Functions where

import Prelude hiding (length, map, filter, any, drop, (++), reverse)


length :: [a] -> Int
length [] = 0
length (_ : xs) = 1 + length xs

map :: (a -> b) -> [a] -> [b]
map f [] = []
map f (x : xs) = f x : map f xs

filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter f (x : xs)
    | f x = x : filter f xs
    | otherwise = filter f xs
    -- case f x of
    --     False -> filter f xs
    --     True -> x : filter f xs
    -- if f x
        -- then x : filter f xs
        -- else filter f xs

any :: (a -> Bool) -> [a] -> Bool
any _ [] = False
any f (x : xs) = f x || any f xs


drop :: Int -> [a] -> [a]
drop _ [] = []
drop i (x : xs) | i <= 0 = (x : xs)
                | otherwise =  drop (i - 1) xs

append :: [a] -> [a] -> [a]
append [] ys = ys
append ys [] = ys
append (x : xs)  ys = x : (append xs ys)

append' :: [a] -> [a] -> [a]
append' [] ys = ys
append' (x : xs)  ys = x : (append' xs ys)

(++) :: [a] -> [a] -> [a]
(++) = append


reverse :: [a] -> [a]
reverse [] = []
reverse (x : xs) = reverse xs ++ [x]

slice :: Int -> Int -> [a] -> [a]
slice i l xs | l <= 0 = []
             | otherwise = take l $ drop i xs

snoc :: [a] -> a -> [a]
snoc xs x = xs ++ [x]

snoc' :: [a] -> a -> [a]
snoc' [] y = [y]
snoc' (x : xs) y = x : snoc' xs y

nub :: Eq a => [a] -> [a]
nub [] = []
nub (x : xs) = x : (nub (filter (/=x) xs))



module Tables where

import Data.Maybe hiding (mapMaybe)
import Control.Applicative
import Prelude hiding (uncurry, curry, zip, zipWith, lookup)

data Pair a b = Pair a b
    deriving Show

fst' :: Pair a b -> a
fst' (Pair x _) = x

uncurry :: (a -> b -> c) -> (a, b) -> c
uncurry f (x, y) = f x y

swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

fromPair :: Pair a b -> (a, b)
fromPair (Pair x y) = (x, y)

toPair :: (a, b) -> Pair a b
toPair (a, b) = Pair a b

curry :: ((a, b) -> c) -> a -> b -> c
curry f x y = f (x, y)

zip :: [a] -> [b] -> [(a, b)]
zip [] _ = []
zip _ [] = []
zip (x : xs) (y : ys) = (x, y) : zip xs ys

-- zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
-- zipWith f (x : xs) (y : ys) = f x y : zipWith f xs ys
-- zipWith _ _ _ = []

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
-- zipWith' f xs ys = [uncurry f i | i <- zip xs ys]
zipWith' f xs ys = map (uncurry f) $ zip xs ys

example :: [(Int, String)]
example =
    [ (1, "Frodo")
    , (2, "Bilbo")
    , (3, "Sauron")
    , (3, "Gandalf")
    , (11, "Legolas")
    , (14, "Gimli")
    , (20, "Boromir")
    ]

example' :: [(Int, String)]
example' = undefined
    -- [ (3, "ABC")]

data LookupResult val = LookupFailed | LookupSuccessful val
    deriving Show

lookup :: (Eq key, Show key) => key -> [(key, val)] -> Maybe val
lookup _ [] = Nothing
lookup key ((key', val) : table)
    | key == key' = Just val
    | otherwise = lookup key table

lookup' :: (Eq key, Show key) => key -> [(key, val)] -> LookupResult val
lookup' key [] = LookupFailed
lookup' key ((key', val) : table)
    | key == key' = LookupSuccessful val
    | otherwise = lookup' key table

delete :: Eq key => key -> [(key, val)] -> [(key, val)]
delete _ [] = []
delete key ((key', val) : table)
    | key == key' = delete key table
    | otherwise = (key', val) : delete key table

delete' :: Eq key => key -> [(key, val)] -> [(key, val)]
delete' key table = filter (\ x -> fst x /= key) table

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : xs) = Just x

maximumAux :: Ord a => a -> [a] -> a
maximumAux x [] = x
maximumAux x (y : ys) = maximumAux (max x y) ys

safeMaximum :: Ord a => [a] -> Maybe a
safeMaximum [] = Nothing
safeMaximum (x : xs) = Just $ maximumAux x xs

-- fromMaybe :: a -> Maybe a -> a
-- fromMaybe def Nothing = def
-- fromMaybe _ (Just x) = x

mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapMaybe f Nothing = Nothing
mapMaybe f (Just x) = Just $ f x

orelse :: Maybe a -> Maybe a -> Maybe a
orelse Nothing y = y
orelse (Just x) _ = Just x

extract' :: String -> Char
extract' x = fromMaybe ' ' (safeHead x)

alts :: [Maybe a] -> Maybe a
alts [] = Nothing
alts (Nothing : ys) = alts ys
alts ((Just x) : _) = Just x 

module Intro where

import Prelude hiding (not, (||), elem)

ten = five + five
five = 2 + 3
aList = [1,2,3,4,five]

six = 3 + 3

seven = 3 + 4

double x = x + x

distance x y = abs $ x - y

data Choice = Rock | Paper | Scissors
    deriving Show

improve :: Choice -> Choice
improve Rock = Paper
improve Paper = Scissors
improve Scissors = Rock

not :: Bool -> Bool
not True = False
not False = True

(||) :: Bool -> Bool -> Bool
False || y = y
True  || y = True

infixr 2 ||

data List a = Nil | Cons a (List a)
    deriving Show

sampleList :: List Int
sampleList = Cons 1 $ Cons 2 $ Cons 3 Nil

elem' :: Eq a => a -> List a -> Bool
elem' x Nil = False
elem' x (Cons y ys) = y == x || elem' x ys

elem :: Eq a => a -> [a] -> Bool
elem x [] = False
elem x (y : ys) = y == x || elem x ys

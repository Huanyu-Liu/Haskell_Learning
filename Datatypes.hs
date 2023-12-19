module Datatypes where

data Tree a = Leaf a | Node (Tree a) (Tree a)
    deriving (Show, Eq)

exampleTree :: Tree Int
exampleTree = Node (Leaf 2) (Leaf 3)

exampleTree2 :: Tree Int
exampleTree2 = Node (Node (Leaf 4) (Leaf 5)) (Leaf 6)

data Choice = Rock | Paper | Scissors

data User' = User' UserId String String
data User = User {uid :: UserId, uname :: String, uemail :: Email}
    deriving Show

newtype UserId = UserId {getUser :: Int}
    deriving Show

uid1 = UserId 42
uid2 = UserId 21

data Email = Email { getEmail :: String}
    deriving Show
type IntList = [Int]

newtype Postal = Postal Int

type Table key val = (key, val)

x :: Table Int Int 
x = (2, 4)

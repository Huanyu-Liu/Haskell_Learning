module Parametricity where

import Prelude hiding (fst, map)

-- fst :: (Int, Bool) -> Int
-- fst :: (a, b) -> a
-- fst :: ()
-- fst (x, _) = x


-- (Int, Int) -> (Int, Int)

f1 :: (Int, Int) -> (Int, Int)
f1 (x, y) = (x + y, x * y)

-- (a, a) -> (a, a)

g1 :: (a, a) -> (a, a)
g1 (x, y) = (x, y)


g2 :: (a, a) -> (a, a)
g2 (x, y) = (y, x)

g3 :: (a, a) -> (a, a)
g3 (x, _) = (x, x)

g4 :: (a, a) -> (a, a)
g4 (_, y) = (y, y)

-- (a, b) -> (b, a)

h :: (a, b) -> (b, a)
h (x, y) = (y, x)

map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x : xs) = f x : map f xs

parse :: String -> ParseResult
parse "False" = ABool False
parse "0" = AnInt 0
parse _ = AString "Unknown"

data ParseResult = ABool Bool | AnInt Int | AString String
    deriving Show


data Expr =
      Lit Int
    | Add Expr Expr
    | Neg Expr
    | And Expr Expr
    | Not Expr
    | Eq Expr Expr
    | If Expr Expr Expr

data Value = IntVal Int | BoolVal Bool | Error
    deriving Show

eval :: Expr -> Value
eval (Lit x) = IntVal x
eval (Add x y) = case (eval x, eval y) of
    ((IntVal intX), (IntVal intY)) -> IntVal (intX + intY)
    (_, _) -> Error
eval (Neg x) = case (eval x) of
    IntVal intX -> IntVal (-intX)
    _ -> Error

eval (And x y) = case (eval x, eval y) of
    (BoolVal boolX, BoolVal booly) -> BoolVal (boolX && booly)
    (_, _) -> Error
eval (Not x) = case (eval x) of
    BoolVal boolX -> BoolVal (not boolX)
    _ -> Error
eval (Eq x y) = case (eval x, eval y) of
    (IntVal intX, IntVal intY) -> BoolVal (intX == intY)
    (_, _) -> Error
eval (If x y z) = case eval x of
    BoolVal True -> eval y
    BoolVal False -> eval z
    _ -> Error


expr1 :: Expr
expr1 = Neg (Add (Lit 3) (Lit 5))

expr3 :: Expr
expr3 = Eq expr1 expr1

expr2 :: Expr
expr2 = If expr3 (Lit 1) (Lit 0)

-- prop_eval1 :: Bool
-- prop_eval1 = eval expr1 == -8

-- prop_eval2 :: Bool
-- prop_eval2 = eval expr2 == 0

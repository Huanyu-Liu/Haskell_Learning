module Parametricity where

import Prelude hiding (fst)

-- fst :: (Int, Bool) -> Int
-- fst :: (a, b) -> a
fst :: ()
fst (x, _) = x


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

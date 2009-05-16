{-# LANGUAGE GADTs, FlexibleInstances, FlexibleContexts #-}
module Integer where

import FRP

instance Str Int where
  string = prim "/*cast*/"

-- Numeric instances for integer nodes.

instance Num (Node a Int) where
  (+) = prim2 "add"
  (*) = prim2 "mul"
  (-) = prim2 "sub"
  abs = prim "abs"
  signum = prim "sign"
  fromInteger a = Const (show a)

instance Fractional (Node a Int) where
  (/) = prim2 "div"
  fromRational r = Const (show r)

mod :: Node a Int -> Node a Int -> Node a Int
mod = prim2 "mod"

max :: Node a Int -> Node a Int -> Node a Int
max = prim2 "max"

min :: Node a Int -> Node a Int -> Node a Int
min = prim2 "min"

-- Booleans.
instance Str Bool where
  string = prim "/*cast*/"

_if :: Node a Bool -> Node a b -> Node a b -> Node a b
_if = prim3 "_if"


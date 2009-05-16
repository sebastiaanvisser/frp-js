{-# LANGUAGE GADTs, FlexibleInstances, FlexibleContexts #-}
module Integer where

import FRP

instance Str Int where
  string = prim "/*cast*/"

-- Numeric instances for integer nodes.

instance Num (Node a Int) where
  (+) = prim2 "lift(function(a,b)a+b)"
  (*) = prim2 "lift(function(a,b)a*b)"
  (-) = prim2 "lift(function(a,b)a-b)"
  abs = prim "lift(Math.abs)"
  signum = prim "lift(Math.sign)"
  fromInteger a = Const (show a)

instance Fractional (Node a Int) where
  (/) = prim2 "div"
  fromRational r = Const (show r)

mod :: Node a Int -> Node a Int -> Node a Int
mod = prim2 "lift(function(a,b)a%b)"

max :: Node a Int -> Node a Int -> Node a Int
max = prim2 "lift(Math.max)"

min :: Node a Int -> Node a Int -> Node a Int
min = prim2 "lift(Math.min)"

-- Booleans.
instance Str Bool where
  string = prim "/*cast*/"

_if :: Node a Bool -> Node a b -> Node a b -> Node a b
_if = prim3 "lift(function(c,i,e)c?i:e)"


{-# LANGUAGE FlexibleInstances, TypeOperators #-}
module Value.Number where

import Core.Val

instance ToText Number where
  text = prim "/*cast*/"

instance Num (Val Number) where
  (+) = prim2 "combine(function(a,b)a+b)"
  (*) = prim2 "combine(function(a,b)a*b)"
  (-) = prim2 "combine(function(a,b)a-b)"
  abs = prim "combine(Math.abs)"
  signum = prim "combine(Math.sign)"
  fromInteger = Const . show

instance Fractional (Val Number) where
  (/) = prim2 "combine(function(a,b)a/b)"
  fromRational = Const . show

mod :: Number :->: Number :~>: Number
mod = prim2 "combine(function(a,b)a%b)"

max :: Number :->: Number :~>: Number
max = prim2 "combine(Math.max)"

min :: Number :->: Number :~>: Number
min = prim2 "combine(Math.min)"

sin :: Number :~>: Number
sin = prim "combine(Math.sin)"

cos :: Number :~>: Number
cos = prim "combine(Math.cos)"

sqrt :: Number :~>: Number
sqrt = prim "combine(Math.sqrt)"

infix  4 <:, <=:, >=:, >:

(>:) :: Number :->: Number :~>: Boolean
(>:) = prim2 "combine(function(a,b)a>b)"

(>=:) :: Number :->: Number :~>: Boolean
(>=:) = prim2 "combine(function(a,b)a>=b)"

(<:) :: Number :->: Number :~>: Boolean
(<:) = prim2 "combine(function(a,b)a<b)"

(<=:) :: Number :->: Number :~>: Boolean
(<=:) = prim2 "combine(function(a,b)a<=b)"


{-# LANGUAGE FlexibleInstances, TypeOperators #-}
module Value.Number where

import Core.Val

instance ToText Number where
  text = prim "$(function(x){return x})"

instance Num (Val Number) where
  (+) = prim2 "$(function(a,b){return a+b})"
  (*) = prim2 "$(function(a,b){return a*b})"
  (-) = prim2 "$(function(a,b){return a-b})"
  abs = prim "$(Math.abs)"
  signum = prim "$(Math.sign)"
  fromInteger = Const . show

instance Fractional (Val Number) where
  (/) = prim2 "$(function(a,b){return a/b})"
  fromRational = Const . show

mod :: Number :->: Number :~>: Number
mod = prim2 "$(function(a,b){return a%b})"

max :: Number :->: Number :~>: Number
max = prim2 "$(Math.max)"

min :: Number :->: Number :~>: Number
min = prim2 "$(Math.min)"

sin :: Number :~>: Number
sin = prim "$(Math.sin)"

cos :: Number :~>: Number
cos = prim "$(Math.cos)"

sqrt :: Number :~>: Number
sqrt = prim "$(Math.sqrt)"

infix  4 <:, <=:, >=:, >:

(>:) :: Number :->: Number :~>: Boolean
(>:) = prim2 "$(function(a,b){return a>b})"

(>=:) :: Number :->: Number :~>: Boolean
(>=:) = prim2 "$(function(a,b){return a>=b})"

(<:) :: Number :->: Number :~>: Boolean
(<:) = prim2 "$(function(a,b){return a<b})"

(<=:) :: Number :->: Number :~>: Boolean
(<=:) = prim2 "$(function(a,b){return a<=b})"


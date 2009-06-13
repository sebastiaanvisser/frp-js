{-# LANGUAGE FlexibleInstances, TypeOperators #-}
module Value.Number where

import Core.Val

instance ToText Number where
  text = prim Fun "id" "$(function(x){return x})"

instance Num (Val Number) where
  (+) = prim2 Fun "+" "$(function(a,b){return a+b})"
  (*) = prim2 Fun "*" "$(function(a,b){return a*b})"
  (-) = prim2 Fun "-" "$(function(a,b){return a-b})"
  abs = prim Fun "abs" "$(Math.abs)"
  signum = prim Fun "sign" "$(Math.sign)"
  fromInteger i = Prim Con (show i) (show i)

instance Fractional (Val Number) where
  (/) = prim2 Fun "/" "$(function(a,b){return a/b})"
  fromRational r = Prim Con (show r) (show r)

mod :: Number :-> Number :~> Number
mod = prim2 Fun "%" "$(function(a,b){return a%b})"

max :: Number :-> Number :~> Number
max = prim2 Fun "max" "$(Math.max)"

min :: Number :-> Number :~> Number
min = prim2 Fun "min" "$(Math.min)"

sin :: Number :~> Number
sin = prim Fun "sin" "$(Math.sin)"

cos :: Number :~> Number
cos = prim Fun "cos" "$(Math.cos)"

sqrt :: Number :~> Number
sqrt = prim Fun "sqrt" "$(Math.sqrt)"

floor :: Number :~> Number
floor = prim Fun "floor" "$(Math.floor)"

infix  4 <, <=, >=, >

(>) :: Number :-> Number :~> Boolean
(>) = prim2 Fun ">" "$(function(a,b){return a>b})"

(>=) :: Number :-> Number :~> Boolean
(>=) = prim2 Fun ">=" "$(function(a,b){return a>=b})"

(<) :: Number :-> Number :~> Boolean
(<) = prim2 Fun "<" "$(function(a,b){return a<b})"

(<=) :: Number :-> Number :~> Boolean
(<=) = prim2 Fun "<=" "$(function(a,b){return a<=b})"


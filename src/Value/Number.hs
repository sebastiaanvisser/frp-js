{-# LANGUAGE FlexibleInstances #-}
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
  fromInteger a = Const (show a)

instance Fractional (Val Number) where
  (/) = prim2 "combine(function(a,b)a/b)"
  fromRational r = Const (show r)

mod :: Val Number -> Val Number -> Val Number
mod = prim2 "combine(function(a,b)a%b)"

max :: Val Number -> Val Number -> Val Number
max = prim2 "combine(Math.max)"

min :: Val Number -> Val Number -> Val Number
min = prim2 "combine(Math.min)"

sin :: Val Number -> Val Number
sin = prim "combine(Math.sin)"

cos :: Val Number -> Val Number
cos = prim "combine(Math.cos)"

sqrt :: Val Number -> Val Number
sqrt = prim "combine(Math.sqrt)"

infix  4 <:, <=:, >=:, >:

(>:) :: Val Number -> Val Number -> Val Boolean
(>:) = prim2 "combine(function(a,b)a>b)"

(>=:) :: Val Number -> Val Number -> Val Boolean
(>=:) = prim2 "combine(function(a,b)a>=b)"

(<:) :: Val Number -> Val Number -> Val Boolean
(<:) = prim2 "combine(function(a,b)a<b)"

(<=:) :: Val Number -> Val Number -> Val Boolean
(<=:) = prim2 "combine(function(a,b)a<=b)"


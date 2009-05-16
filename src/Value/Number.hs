{-# LANGUAGE FlexibleInstances #-}
module Value.Number where

import Core.Val

instance ToText Number where
  text = prim "/*cast*/"

instance Num (Val Number) where
  (+) = prim2 "lift(function(a,b)a+b)"
  (*) = prim2 "lift(function(a,b)a*b)"
  (-) = prim2 "lift(function(a,b)a-b)"
  abs = prim "lift(Math.abs)"
  signum = prim "lift(Math.sign)"
  fromInteger a = Const (show a)

instance Fractional (Val Number) where
  (/) = prim2 "lift(function(a,b)a/b)"
  fromRational r = Const (show r)

mod :: Val Number -> Val Number -> Val Number
mod = prim2 "lift(function(a,b)a%b)"

max :: Val Number -> Val Number -> Val Number
max = prim2 "lift(Math.max)"

min :: Val Number -> Val Number -> Val Number
min = prim2 "lift(Math.min)"

sin :: Val Number -> Val Number
sin = prim "lift(Math.sin)"


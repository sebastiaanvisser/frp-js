{-# LANGUAGE TypeOperators #-}
module Value.List where

import Core.Val

instance ToText a => ToText (List a) where
  text = prim Fun "id" "$(function(x){return x})"

index :: Number :-> List a :~> a
index = prim2 Fun "!!" "$(function(i,x){return x ? x[i] : this.v})"

sort :: List a :~> List a
sort = prim Fun "sort" "$(function(a){return a.sort()})"

-- sortBy :: Val (a :~> b) -> Val (List a) :~> Val (List a)
-- sortBy = prim2 Fun "$(Array.sort)"

reverse :: List a :~> List a
reverse = prim Fun "rev" "$(function(a){return a.reverse()})"

switch :: Val b -> [Val a] -> Val a
switch a xs = Prim Fun "switch" "_switch" `App` a `App` Comb xs

alternate :: Val b -> [Val a] -> Val a
alternate a xs = a `switch` concat (zipWith (\x y -> [x, y]) xs xs)

fromto :: Boolean :-> Boolean :~> Boolean
fromto = prim2 Fun "..." "fromto"

on :: b :~> Boolean
on a = a `alternate` [con True, con False]


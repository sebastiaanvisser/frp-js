{-# LANGUAGE TypeOperators #-}
module Value.List where

import Core.Val

instance ToText a => ToText (List a) where
  text = prim "/*cast*/"

sort :: List a :~>: (List a)
sort = prim "combine(Array.sort)"

reverse :: List a :~>: List a
reverse = prim "combine(Array.reverse)"

switch :: Val b -> [Val a] -> Val a
switch a xs = Prim "_switch" `App` a `App` Comb xs

alternate :: Val b -> [Val a] -> Val a
alternate a xs = a `switch` concat (zipWith (\x y -> [x, y]) xs xs)

on :: b :~>: Boolean
on a = a `alternate` [con True, con False]


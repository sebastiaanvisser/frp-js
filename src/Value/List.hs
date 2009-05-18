module Value.List where

import Core.Val

instance ToText a => ToText (List a) where
  text = prim "/*cast*/"

sort :: Val (List a) -> Val (List a)
sort = prim "combine(Array.sort)"

reverse :: Val (List a) -> Val (List a)
reverse = prim "combine(Array.reverse)"

switch ::  Val b -> [Val a] -> Val a
switch a xs = Prim "_switch" `App` a `App` Comb xs

alternate :: Val b -> [Val a] -> Val a
alternate a xs = a `switch` concat (zipWith (\x y -> [x, y]) xs xs)

on :: Val b -> Val Boolean
on a = a `alternate` [con True, con False]


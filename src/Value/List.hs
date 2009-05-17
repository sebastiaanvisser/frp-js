module Value.List where

import Core.Val

instance ToText a => ToText (List a) where
  text = prim "/*cast*/"

sort :: Val (List a) -> Val (List a)
sort = prim "lift(Array.sort)"

reverse :: Val (List a) -> Val (List a)
reverse = prim "lift(Array.reverse)"

switch :: [Val a] -> Val b -> Val a
switch xs a = Prim "_switch" `App` a `App` Comb xs

alternate :: [Val a] -> Val b -> Val a
alternate xs a = concat (zipWith (\x y -> [x, y]) xs xs) `switch` a


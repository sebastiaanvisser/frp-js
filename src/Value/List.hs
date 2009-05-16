module Value.List where

import Core.Val

instance Str Char where
  string = prim "/*cast*/"

instance Str a => Str [a] where
  string = prim "/*cast*/"

sort :: Val [b] -> Val [b]
sort = prim "lift(Array.sort)"

reverse :: Val [b] -> Val [b]
reverse = prim "lift(Array.reverse)"


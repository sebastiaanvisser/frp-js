module List where

import FRP

instance Str Char where
  string = prim "/*cast*/"

instance Str a => Str [a] where
  string = prim "/*cast*/"

sort :: Node a [b] -> Node a [b]
sort = prim "sort"

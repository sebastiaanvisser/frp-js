module Value.List where

import Core.Val

instance ToText a => ToText (List a) where
  text = prim "/*cast*/"

sort :: Val (List a) -> Val (List a)
sort = prim "lift(Array.sort)"

reverse :: Val (List a) -> Val (List a)
reverse = prim "lift(Array.reverse)"


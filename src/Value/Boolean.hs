module Value.Boolean where

import Core.Val

instance ToText Boolean where
  text = prim "/*cast*/"

_if :: Val Boolean -> Val a -> Val a -> Val a
_if = prim3 "lift(function(c,i,e)c?i:e)"


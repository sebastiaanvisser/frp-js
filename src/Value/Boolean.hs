module Value.Boolean where

import Core.Val

instance ToText Boolean where
  text = prim "/*cast*/"

_if :: Val Boolean -> Val a -> Val a -> Val a
_if = prim3 "combine(function(c,i,e)c?i:e)"

while :: Val a -> Val Boolean -> Val a
while = prim2 "combine(function(i,c)c?i:this.v)"

infix  4  ==:, !=:
infixr 3  &&
infixr 2  ||

not :: Val Boolean -> Val Boolean
not = prim "combine(function(a)!a)"

(==:) :: Val a -> Val a -> Val Boolean
(==:) = prim2 "combine(function(a,b)a==b)"

(!=:) :: Val a -> Val a -> Val Boolean
(!=:) = prim2 "combine(function(a,b)a!=b)"

(&&) :: Val Boolean -> Val Boolean -> Val Boolean
(&&) = prim2 "combine(function(a,b)a&&b)"

(||) :: Val Boolean -> Val Boolean -> Val Boolean
(||) = prim2 "combine(function(a,b)a||b)"


{-# LANGUAGE TypeOperators #-}
module Value.Boolean where

import Core.Val

instance ToText Boolean where
  text = prim Cast "cast" "/*cast*/"

_if :: Boolean :-> a :-> a :~> a
_if = prim3 Fun "if" "$(function(c,i,e){return c?i:e})"

while :: a :-> Boolean :~> a
while = prim2 Fun "while" "$(function(i,c){return c?i:this.v})"

infix  4  ==, !=
infixr 3  &&
infixr 2  ||

not :: Boolean :~> Boolean
not = prim Fun "not" "$(function(a){return !a})"

(==) :: a :-> a :~> Boolean
(==) = prim2 Fun "==" "$(function(a,b){return a==b})"

(!=) :: a :-> a :~> Boolean
(!=) = prim2 Fun "!=" "$(function(a,b){return a!=b})"

(&&) :: Boolean :-> Boolean :~> Boolean
(&&) = prim2 Fun "&&" "$(function(a,b){return a&&b})"

(||) :: Boolean :-> Boolean :~> Boolean
(||) = prim2 Fun "||" "$(function(a,b){return a||b})"


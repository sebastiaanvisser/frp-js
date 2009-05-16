module Geometry where

import Val
import Number

class Point a where
  px :: a -> Val Number
  py :: a -> Val Number

class Geometry a where
  left   :: a -> Val Number
  top    :: a -> Val Number
  width  :: a -> Val Number
  height :: a -> Val Number

bottom :: Geometry a => a -> Val Number
bottom a = top a + height a

right :: Geometry a => a -> Val Number
right a = left a + width a

corners :: Geometry a => a -> [(Val Number, Val Number)]
corners a = 
  [ (left  a, top    a)
  , (right a, top    a)
  , (left  a, bottom a)
  , (right a, bottom a)
  ]

geom
  :: Geometry a
  => (Val Number, Val Number, Val Number, Val Number)
  -> a -> FRP ()
geom (a, b, c, d) o =
  do left   o <-: a
     top    o <-: b
     width  o <-: c
     height o <-: d

overlay :: (Geometry a, Geometry b) => a -> b -> Val Number -> FRP ()
overlay a b c =
  do left   a <-: left   b + c
     top    a <-: top    b + c
     width  a <-: width  b - c * 2
     height a <-: height b - c * 2


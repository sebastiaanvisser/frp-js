{-# LANGUAGE FlexibleInstances, TypeOperators #-}
module Property.Geometry where

import Prelude hiding ((&&), (||), max, min, sqrt)
import Core.Val
import Value.Number
import Value.Boolean

class Point a where
  px :: a -> Val Number
  py :: a -> Val Number

instance Point (Val Number, Val Number) where
  px = fst
  py = snd

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

geom :: Geometry a => (Val Number, Val Number, Val Number, Val Number) -> a -> FRP ()
geom (a, b, c, d) o =
  do left   o <~ a
     top    o <~ b
     width  o <~ c
     height o <~ d

overlay :: (Geometry a, Geometry b) => a -> b -> Number :->: FRP ()
overlay a b c =
  do left   a <~ left   b + c
     top    a <~ top    b + c
     width  a <~ width  b - c * 2
     height a <~ height b - c * 2

inside :: (Point a, Geometry b) => a -> b -> Val Boolean
inside a b =
     px a >=: left   b
  && px a <=: right  b
  && py a >=: top    b
  && py a <=: bottom b

collapse :: (Geometry a, Geometry b) => a -> b -> Val Boolean
collapse a b =
  let x = (left   a `max` left   b)
      y = (top    a `max` top    b)
      w = (right  a `min` right  b) - x 
      h = (bottom a `min` bottom b) - y 
  in x >: 0 && y >: 0 && w >: 0 && h >: 0

distance :: (Point a, Point b) => a -> b -> Val Number
distance a b = sqrt
  ( (px a - px b) * (px a - px b)
  + (py a - py b) * (py a - py b)
  )


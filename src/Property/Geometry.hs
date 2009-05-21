{-# LANGUAGE FlexibleInstances, TypeOperators #-}
module Property.Geometry where

import Prelude hiding ((&&), (||), max, min, sqrt, (<), (<=), (>), (>=))
import Core.Val
import Value.Number
import Value.Boolean


data Point = Point 
  { px :: Val Number
  , py :: Val Number
  }

class Position a where
  position :: a -> Point

instance Position Point where
  position = id

data Rect = Rect
  { left   :: Val Number
  , top    :: Val Number
  , width  :: Val Number
  , height :: Val Number
  }

class Geometry a where
  geometry :: a -> Rect

bottom :: Rect -> Val Number
bottom a = top a + height a

right :: Rect -> Val Number
right a = left a + width a

center :: Rect -> Point
center a = Point (left a + width a / 2) (top a + height a / 2)

corners :: Rect -> [Point]
corners a = 
  [ Point (left  a) (top    a)
  , Point (right a) (top    a)
  , Point (left  a) (bottom a)
  , Point (right a) (bottom a)
  ]

setGeom :: Rect -> Rect -> FRP ()
setGeom a b =
  do left   a <~ left   b
     top    a <~ top    b
     width  a <~ width  b
     height a <~ height b

overlay :: Rect -> Rect -> Val Number -> FRP ()
overlay a b c =
  do left   a <~ left   b + c
     top    a <~ top    b + c
     width  a <~ width  b - c * 2
     height a <~ height b - c * 2

inside :: Point -> Rect -> Val Boolean
inside a b =
     px a >= left   b
  && px a <= right  b
  && py a >= top    b
  && py a <= bottom b

collapse :: Rect -> Rect -> Val Boolean
collapse a b =
  let x = (left   a `max` left   b)
      y = (top    a `max` top    b)
      w = (right  a `min` right  b) - x 
      h = (bottom a `min` bottom b) - y 
  in x > 0 && y > 0 && w > 0 && h > 0

distance :: Point -> Point -> Val Number
distance a b = sqrt
  ( (px a - px b) * (px a - px b)
  + (py a - py b) * (py a - py b)
  )


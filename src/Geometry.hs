module Geometry where

import FRP
import Integer ()

class Point a where
  px :: a -> Node Int Int
  py :: a -> Node Int Int

class Geometry a where
  left   :: a -> Node Int Int
  top    :: a -> Node Int Int
  width  :: a -> Node Int Int
  height :: a -> Node Int Int

bottom :: Geometry a => a -> Node Int Int
bottom a = top a + height a

right :: Geometry a => a -> Node Int Int
right a = left a + width a

geom
  :: Geometry a
  => (Node a Int, Node b Int, Node c Int, Node d Int)
  -> a -> FRP ()
geom (a, b, c, d) o =
  do left   o <-: a
     top    o <-: b
     width  o <-: c
     height o <-: d

attachBottom :: (Geometry a, Geometry b) => a -> b -> FRP ()
attachBottom a b =
  do width a <-: width  b
     left  a <-: left   b
     top   a <-: bottom b


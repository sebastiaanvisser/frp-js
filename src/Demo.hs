{-# LANGUAGE TypeOperators #-}
module Demo (demo) where

import Browser.Element
import Browser.Time
import Browser.Mouse
import Core.Val
import Prelude hiding (mod, max, min, reverse, sin, (&&), (||), not, sin, cos, floor)
import Property.Color
import Property.Geometry
import Property.Text
import Value.Boolean
import Value.List
import Value.Number

demo :: FRP ()
demo =
  do let target = ById "a"

     geometry target `setGeom` Rect 400 400 200 200 
     visible target <~ con True
     color target   <~ con "orange"

     draggable target
     color target <~ _if (dragging target) (con "red") (con "orange")
  
dragging :: Geometry b => b -> Val Boolean
dragging t = fromto 
  (down Mouse && position Mouse `inside` geometry t)
  (not (down Mouse))

draggable :: Geometry b => b -> FRP ()
draggable t =
  do let g = geometry t
         m = position Mouse
         d = dragging t
         x = (px m - left g) `while` not d
         y = (py m - top  g) `while` not d
     left g <~ (- x + px m) `while` d
     top  g <~ (- y + py m) `while` d






-- pulse
--   :: Geometry a
--   => a
--   -> Number :-> Number :-> Number
--  :-> Number :-> Number :-> FRP ()
-- pulse a x y f t ss =
--   do let s = f + t * sin (time / ss)
--      left   a <~ x - width  a / 2
--      top    a <~ y - height a / 2
--      width  a <~ s
--      height a <~ s


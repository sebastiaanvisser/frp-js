{-# LANGUAGE TypeOperators #-}
module Demo2 (demo) where

import Browser.Element
import Browser.Time
import Browser.Mouse
import Core.Val
import Prelude hiding (mod, max, min, reverse, sin, (&&), (||), not, sin, cos)
import Property.Color
import Property.Geometry
import Property.Text
import Value.Boolean
import Value.List ()
import Value.Number

demo :: FRP ()
demo =
  do let target = ById "a"
         boxes = map (ById . ('d':) . show) [0..(19::Int)]

     -- The orange target dot is always following the mouse cursor, but gives
     -- some space to links in the top of the document.
     geometry target `setGeom` Rect 400 400 40 40
     visible target <~ con True
     color target   <~ con "orange"
     left (geometry target) <~ (px (position Mouse) - width  (geometry target) / 2)
     top  (geometry target) <~ (py (position Mouse) - height (geometry target) / 2) `max` 120

     -- Window title is the distance between mouse curos and top-left part of
     -- the document.
     textVal Document <~ text (position Mouse `distance` Point 0 0)

     -- Render some gray boxes in a circle, let the all pulse repetitively.
     -- When they come in contact with the target they grow and become red!
     flip mapM_ (zip boxes [0..]) $ \(b, i) ->
       do let j = con (i::Int)
              x = 400 + 250 * cos j
              y = 400 + 250 * sin j
              c = geometry target `collapse` geometry b
         
          visible b   <~ con True
          left (geometry target) <~ x
          top  (geometry target) <~ y
          color b     <~ _if c (con "red") (con "silver")
          pulse b x y (20 + (position Mouse `distance` Point x y) / 3) 20 200
  where
    pulse a x y f t ss =
      do let s = f + t * (sin (time / ss))
         left   (geometry a) <~ x - width  (geometry a) / 2
         top    (geometry a) <~ y - height (geometry a) / 2
         width  (geometry a) <~ s
         height (geometry a) <~ s


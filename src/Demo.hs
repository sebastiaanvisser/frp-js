{-# LANGUAGE TypeOperators #-}
module Demo (demo) where

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
         boxes = map (ById . ('d':) . show) [0..(9::Int)]

     -- The orange target dot is always following the mouse cursor, but gives
     -- some space to links in the top of the document.
     geom (400, 400, 40, 40) target
     visible target <~ con True
     color target   <~ con "orange"
     left target    <~ (px Mouse - width  target / 2)
     top  target    <~ (py Mouse - height target / 2) `max` 80

     -- Window title is the distance between mouse curos and top-left part of
     -- the document.
     let orig = (0::Val Number, 0::Val Number)
     textVal Document <~ text (distance Mouse orig)

     -- Render some gray boxes in a circle, let the all pulse repetitively.
     -- When they come in contact with the target they grow and become red!
     flip mapM_ (zip boxes [0..]) $ \(b, i) ->
       do let j = con (i::Int)
              x = 400 + 250 * cos j
              y = 400 + 250 * sin j
              c = collapse target b
         
          visible b   <~ con True
          left target <~ x
          top  target <~ y
          color b     <~ _if c (con "red") (con "silver")
          pulse b x y (_if c 120 40) 20 200

pulse
   ::  Geometry a
   =>  a
   ->  Number :->: Number :->: Number
  :->: Number :->: Number :->: FRP ()
pulse a x y f t ss =
  do let s = f + t * (sin (time / ss))
     left   a <~ x - width  a / 2
     top    a <~ y - height a / 2
     width  a <~ s
     height a <~ s



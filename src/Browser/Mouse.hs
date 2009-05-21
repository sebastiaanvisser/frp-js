module Browser.Mouse where

import Property.Geometry
import Core.Val

data Mouse = Mouse

instance Position Mouse where
  position _ = Point (Prim "mouseX") (Prim "mouseY")

down :: Mouse -> Val Boolean
down _ = Prim "mouseDown"


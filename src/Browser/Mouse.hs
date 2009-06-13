module Browser.Mouse where

import Property.Geometry
import Core.Val

data Mouse = Mouse

instance Position Mouse where
  position _ = Point (Prim In "mouse.x" "mouseX") (Prim In "mouse.y" "mouseY")

down :: Mouse -> Val Boolean
down _ = Prim In "mouse.down" "mouseDown"


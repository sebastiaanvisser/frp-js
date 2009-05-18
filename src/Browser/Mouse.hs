module Browser.Mouse where

import Property.Geometry
import Core.Val

data Mouse = Mouse

instance Point Mouse where
  px _ = Prim "mouseX"
  py _ = Prim "mouseY"

down :: Mouse -> Val Boolean
down _ = Prim "mouseDown"


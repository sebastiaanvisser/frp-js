{-# LANGUAGE EmptyDataDecls, FlexibleInstances, OverlappingInstances, IncoherentInstances #-}
module Window where

import FRP
import Geometry
import Text

class Color a where
  color :: a -> Node String String

-- Mouse input.

data Mouse = Mouse

instance Point Mouse where
  px _ = Prim "mouse.x"
  py _ = Prim "mouse.y"

down :: Mouse -> Node Bool Bool
down _ = Prim "mouse.down"

-- Window title output.

data Window = Window

instance Text Window where
  text _ = Prim "title"

-- DOM elements.

getElem :: Element t -> String
getElem (ById i) = concat ["document.getElementById('", i, "')"]
getElem Body     = "document.body"

property :: Element t -> String -> String -> Node a b
property e s p = Prim $ concat ["property(", getElem e, s, ",'", p, "')"]

event :: Element t -> String -> String -> String -> Node a b
event e s p ev = Prim $ concat ["propEvent(", getElem e, s, ",'", p, "','", ev, "')"]

data Element a =
    ById String
  | Body

instance Geometry (Element t) where
  left   i = property i ".style" "left"
  top    i = property i ".style" "top"
  width  i = property i ".style" "width"
  height i = property i ".style" "height"

instance Color (Element t) where
  color i = property i ".style" "backgroundColor"

instance Text (Element t) where
  text i = property i "" "innerHTML"

data Input

input :: String -> Element Input
input i = ById i

instance Text (Element Input) where
  text i = event i "" "value" "onkeyup"


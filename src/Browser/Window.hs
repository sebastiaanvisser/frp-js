{-# LANGUAGE EmptyDataDecls, FlexibleInstances, OverlappingInstances, IncoherentInstances #-}
module Browser.Window where

import Property.Geometry
import Property.Text
import Core.Val

class Color a where
  color :: a -> Val Text

-- Mouse input.

data Mouse = Mouse

instance Point Mouse where
  px _ = Prim "mouse.x"
  py _ = Prim "mouse.y"

down :: Mouse -> Val Boolean
down _ = Prim "mouse.down"

-- Window title output.

data Window = Window

instance TextVal Window where
  textVal _ = Prim "title"

-- DOM elements.

getElem :: Element t -> String
getElem (ById i) = concat ["document.getElementById('", i, "')"]
getElem Body     = "document.body"

property :: Element t -> String -> String -> Val b
property e s p = Prim $ concat ["property(", getElem e, s, ",'", p, "')"]

event :: Element t -> String -> String -> String -> Val a
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

instance TextVal (Element t) where
  textVal i = property i "" "innerHTML"

data Input

input :: String -> Element Input
input i = ById i

instance TextVal (Element Input) where
  textVal i = event i "" "value" "onkeyup"


time :: Val Number
time = Prim "time"



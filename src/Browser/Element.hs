{-# LANGUAGE FlexibleInstances, OverlappingInstances, IncoherentInstances #-}
module Browser.Element where

import Property.Color
import Property.Geometry
import Property.Text
import Core.Val

getElem :: Element t -> String
getElem (ById i) = concat ["document.getElementById('", i, "')"]
getElem Body     = "document.body"
getElem Document = "document"
getElem Window   = "window"

property :: Element t -> String -> String -> Val b
property e s p = Prim $ concat ["property(", getElem e, s, ",'", p, "')"]

event :: Element t -> String -> String -> String -> Val a
event e s p ev = Prim $ concat ["_event(", getElem e, s, ",'", p, "','", ev, "')"]

-- DOM elements.


data Element a =
    ById String
  | Body
  | Document
  | Window

instance Geometry (Element t) where
  left   i = property i ".style" "left"
  top    i = property i ".style" "top"
  width  i = property i ".style" "width"
  height i = property i ".style" "height"

instance Color (Element t) where
  color i = property i ".style" "backgroundColor"

instance Visible (Element t) where
  visible i = property i ".style" "display"
       `Comp` Prim "$(function(a)a?'block':'hidden')"

instance TextVal (Element t) where
  textVal Document = property Document "" "title"
  textVal i        = property i        "" "innerHTML"





data Input = Input

input :: String -> Element Input
input i = ById i

instance TextVal (Element Input) where
  textVal i = event i "" "value" "onkeyup"






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
property e s p = Prim Out (show e ++ s ++ "." ++ p) $ concat ["property(", getElem e, s, ",'", p, "')"]

instance Show (Element a) where
  show (ById s) = s
  show Body     = "body"
  show Document = "document"
  show Window   = "window"

event :: Element t -> String -> String -> String -> Val a
event e s p ev = Prim InOut (show e ++ s ++ "." ++ p) $ concat ["_event(", getElem e, s, ",'", p, "','", ev, "')"]

-- DOM elements.


data Element a =
    ById String
  | Body
  | Document
  | Window

instance Geometry (Element t) where
  geometry i = Rect
    { left   = property i ".style" "left"
    , top    = property i ".style" "top"
    , width  = property i ".style" "width"
    , height = property i ".style" "height"
    }

instance Color (Element t) where
  color   i = property i ".style" "backgroundColor"
  opacity i = property i ".style" "opacity"

instance Visible (Element t) where
  visible i = property i ".style" "display"
       `Comp` Prim Fun "aap" "$(function(a){return a?'block':'hidden'})"

instance TextVal (Element t) where
  textVal Document = property Document "" "title"
  textVal i        = property i        "" "innerHTML"





data Input = Input

input :: String -> Element Input
input i = ById i

instance TextVal (Element Input) where
  textVal i = event i "" "value" "onkeyup"






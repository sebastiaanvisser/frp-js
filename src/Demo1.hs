{-# LANGUAGE TypeOperators #-}
module Demo1 (demo) where

import Browser.Element
import Browser.Mouse
import Core.Val
import Prelude hiding (mod, max, min, reverse, sin, (&&), (||), not, sin, cos, floor, (==))
import Property.Color
import Property.Geometry
import Property.Text
import Value.Boolean
import Value.List

demo :: FRP ()
demo =
  do let doosjes = map (ById . ('d':) . show) [0::Int ..4] :: [Element Input]
         inputs  = map (ById . ('i':) . show) [0::Int ..4] :: [Element Input]
         k = Comb $ map (\i -> Comb [text . left . geometry $ i, textVal i, (\(ById (_:d)) -> con d) i]) doosjes
         first = index 2 (index 0 (sort k))

     flip mapM_ (zip3 doosjes inputs [(0::Int)..]) $ \(d, i, n) ->
       do let c = _if (first == con (show n)) (con "red") (con "silver")
          geometry d `setGeom` Rect (200 + 200 * con n) 400 100 100 
          draggable d
          visible d <~ con True
          textVal d <~> textVal i
          textVal i <~ con ("section " ++ show n)
          color i   <~ c
          color d   <~ _if (dragging d) (con "orange") c
          opacity d <~ _if (dragging d) (con "0.5") (con "1.0")
          color d <~ con "red"

dragging :: Geometry b => b -> Val Boolean
dragging t =
  (down Mouse && position Mouse `inside` geometry t)
  `fromto`
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


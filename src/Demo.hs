module Demo (demo) where

import Browser.Window
import Control.Monad
import Core.Val
import Prelude hiding (mod, max, min, reverse, sin)
import Property.Geometry
import Property.Text
import Value.List
import Value.Boolean
import Value.Number

demo :: FRP ()
demo =
  do let first   = ById "first"
         second  = ById "second"
         third   = ById "third"
         fourth  = ById "fourth"
         fifth   = ById "fifth"
         boxes   = [second, third, fourth, fifth]

         header  = ById "header"

         t1      = input "t1"
         t2      = input "t2"
         t3      = input "t3"
         t4      = input "t4"
         t5      = input "t5"
         
     -- Document title follows mouse cursor.
     textVal Document <-: text (Comb [px Mouse, py Mouse])

     -- Background color depends on mouse state.
     color Body  <-:
       _if (down Mouse)
         (con "green")
         (con "white")

     -- Let header be sorted list of the five input fields.
     let inputs = Comb $ map textVal [t1, t2, t3, t4, t5]
     textVal header <-: text (sort inputs)

     -- Set initial geomtry.
     geom (0, 0, 40,  40) `mapM_` boxes
     geom (0, 0, 400, 300) first

     -- Let first and header follow mouse.
     left first <-: 200 `max` (px Mouse - width  first / 2) `min` 600
     top  first <-: 200 `max` (py Mouse - height first / 2) `min` 400
     overlay header first 20

     -- Switch color when bouncing at left wall.
     color header <-:
      (map con ["yellow", "green", "blue", "orange"])
      `alternate` (left first <=: 220)

     -- Let color boxes follow first.
     mapM_ grow boxes
     zipWithM_ attachCenter boxes (corners first)

-- Helpers.

grow :: Geometry a => a -> FRP ()
grow a =
  do let s = 60 + 40 * sin (time / 200)
     width  a <-: s
     height a <-: s

attachCenter :: Geometry a => a -> (Val Number, Val Number) -> FRP ()
attachCenter a (x, y) =
  do left a <-: x - width  a / 2
     top  a <-: y - height a / 2


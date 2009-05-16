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
  do let red     = ById "first"
         blue    = ById "second"
         orange  = ById "third"
         magenta = ById "fourth"
         pink    = ById "fifth"
         boxes   = [blue, orange, magenta, pink]

         yellow  = ById "header"

         t1      = input "t1"
         t2      = input "t2"
         t3      = input "t3"
         t4      = input "t4"
         t5      = input "t5"
         
     -- Window title follows mouse cursor.
     textVal Window <-: text (Comb [px Mouse, py Mouse])

     -- Background color depends on mouse state.
     color Body  <-:
       _if (down Mouse)
         (con "green")
         (con "white")

     -- Let yellow be sorted list of the five input fields.
     let inputs = Comb $ map textVal [t1, t2, t3, t4, t5]
     textVal yellow <-: text (sort inputs)

     -- Set initial geomtry.
     geom (0, 0, 40,  40) `mapM_` boxes
     geom (0, 0, 400, 300) red

     -- Let red and yellow follow mouse.
     left red <-: 200 `max` (px Mouse - width  red / 2) `min` 600
     top  red <-: 200 `max` (py Mouse - height red / 2) `min` 400
     overlay yellow red 20

     -- Let color boxes follow red.
     mapM_ grow boxes
     zipWithM_ attachCenter boxes (corners red)

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


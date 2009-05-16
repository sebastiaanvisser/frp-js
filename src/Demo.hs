module Demo (demo) where

import Prelude hiding (max, min)
import FRP
import Geometry
import Text
import List
import Window
import Integer

demo :: FRP ()
demo =
  do let red     = ById "first"
         blue    = ById "second"
         orange  = ById "third"
         magenta = ById "fourth"
         pink    = ById "fifth"

         h1      = ById "header"

         t1      = input "t1"
         t2      = input "t2"
         t3      = input "t3"
         t4      = input "t4"
         t5      = input "t5"
         
         subs    = [blue, orange, magenta, pink]

     -- Window title follows mouse cursor.
--      text Window <-: string (down Mouse)
     text Window <-: text t1

     color Body  <-:
       _if (down Mouse)
         (val "green")
         (val "orange")

     text Window <-: string (sort (List (map text [t1, t2, t3, t4, t5])))

     -- Set initial geomtry.
     geom (0, 0, 40, 40) `mapM_` subs
     geom (0, 0, 400, 300) red

     -- Let red follow mouse.
     left red <-: 200 `max` (px Mouse - width  red / 2)
     top  red <-: 200 `max` (py Mouse - height red / 2)

     -- Let color boxes follow red.
     left blue   <-: left red - width blue
     left orange <-: right red
     top magenta <-: top red - height magenta
     top pink    <-: bottom red

     attachBottom h1 red


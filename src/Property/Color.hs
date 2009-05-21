module Property.Color where

import Value.Boolean ()
import Core.Val

class Color a where
  color   :: a -> Val Text
  opacity :: a -> Val Text

class Visible a where
  visible :: a -> Val Boolean


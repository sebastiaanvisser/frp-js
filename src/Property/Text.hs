module Property.Text where

import Core.Val

class TextVal a where
  textVal :: a -> Val Text


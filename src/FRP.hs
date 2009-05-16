{-# LANGUAGE GADTs, KindSignatures, FlexibleInstances, FlexibleContexts #-}
module FRP where

import Prelude hiding ((.), id)
import Control.Category
import Control.Monad.Identity
import Control.Monad.State

data Node :: * -> * -> * where
  Stop  :: Node a b -> Node a ()
  Start :: Node a b -> Node () b
  Comp  :: Node b c -> Node a b -> Node a c
  Prim  :: String -> Node a b
  App   :: Node a (b -> c) -> Node a b -> Node a c
  List  :: [Node a b] -> Node a [b]
  Const :: String -> Node a b

prim :: String -> Node a b -> Node a c
prim f a = Prim f `App` a

prim2 :: String -> Node a b -> Node a c -> Node a d
prim2 f a b = Prim f `App` a `App` b

prim3 :: String -> Node a b -> Node a c -> Node a d -> Node a e
prim3 f a b c = Prim f `App` a `App` b `App` c

instance Eq (Node a b) where
  (==) = undefined

instance Show (Node a b) where
  show = undefined

-- Category instance for easy composing.

instance Category Node where
  (.) = Comp
  id  = Prim "/*id*/"

-- The FRP monad is a state monad that saves dependency graphs.

type FRP a = StateT [Node () ()] Identity a

infixl 2 <-:
infixl 2 <=:

(<-:) :: Node a b -> Node c a -> FRP ()
(<-:) a b = modify ((Stop a . Start b):)

(<=:) :: Show a => Node a b -> a -> FRP ()
(<=:) a b = a <-: Const (show b)

-- Primitive conversions.

class Str b where
  string :: Node a b -> Node a String

-- Lift constant values into nodes.

class Value b where
  val :: b -> Node a b

instance Value [Char] where
  val s = Const (show s)

instance Value Int where
  val i = Const (show i)

instance Value Bool where
  val b = Const (show b)


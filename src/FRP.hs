{-# LANGUAGE GADTs, KindSignatures, FlexibleInstances, FlexibleContexts #-}
module FRP where

import Control.Monad.Identity
import Control.Monad.State

data Val :: * -> * where
--   Stop  :: Val a -> Val ()
--   Start :: Val a -> Val ()
  Conn  :: Val a -> Val a -> Val ()
  Prim  :: String -> Val a
  App   :: Val (a -> b) -> Val a -> Val b
  List  :: [Val a] -> Val [a]
  Const :: String -> Val a

prim :: String -> Val a -> Val b
prim f a = Prim f `App` a

prim2 :: String -> Val a -> Val b -> Val c
prim2 f a b = Prim f `App` a `App` b

prim3 :: String -> Val a -> Val b -> Val c -> Val d
prim3 f a b c = Prim f `App` a `App` b `App` c

instance Eq (Val a) where
  (==) = undefined

instance Show (Val a) where
  show = undefined

-- Category instance for easy composing.

-- The FRP monad is a state monad that saves dependency graphs.

type FRP a = StateT [Val ()] Identity a

infixl 2 <-:
infixl 2 <=:

(<-:) :: Val a -> Val a -> FRP ()
(<-:) a b = modify ((a `Conn` b):)

(<=:) :: Show a => Val a -> a -> FRP ()
(<=:) a b = a <-: Const (show b)

-- Primitive conversions.

class Str a where
  string :: Val a -> Val String

-- Lift constant values into nodes.

class Const a where
  con :: a -> Val a

instance Const [Char] where
  con s = Const (show s)

instance Const Int where
  con i = Const (show i)

instance Const Bool where
  con b = Const (show b)


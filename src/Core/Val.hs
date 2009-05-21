{-# LANGUAGE
    GADTs
  , KindSignatures
  , EmptyDataDecls
  , FlexibleInstances
  , FlexibleContexts
  , MultiParamTypeClasses
  , FunctionalDependencies
  , TypeOperators
 #-}
module Core.Val where

import Control.Monad.Identity
import Control.Monad.State

-- Core types.

data List a
data Number
data Boolean
data Text

-- Indexed FRP values.

data Val :: * -> * where
  App   :: Val (a -> b) -> Val a -> Val b
  Comb  :: [Val a] -> Val (List a)
  Comp  :: Val a -> Val (a -> b) -> Val b
  Conn  :: Val a -> Val a -> Val ()
  Const :: String -> Val a
  Prim  :: String -> Val a

infixr 1 :->
infixr 2 :~>
type a :-> b = Val a -> b
type a :~> b = Val a -> Val b

prim :: String -> a :~> b
prim f a = Prim f `App` a

prim2 :: String -> a :-> b :~> c
prim2 f a b = Prim f `App` a `App` b

prim3 :: String -> a :-> b :-> c :~> d
prim3 f a b c = Prim f `App` a `App` b `App` c

instance Eq (Val a) where
  (==) = undefined

instance Show (Val a) where
  show = undefined

-- Category instance for easy composing.

-- The FRP monad is a state monad that saves dependency graphs.

type FRP a = StateT [Val ()] Identity a

infixl 1 <~

(<~) :: a :-> a :-> FRP ()
(<~) a b = modify (Conn a b:)

-- Primitive conversions.

class ToText a where
  text :: Val a -> Val Text

instance ToText Text where
  text = prim "/*cast*/"

-- Lift constant values into nodes.

class Show a => Const a b | a -> b where
  con :: a -> Val b

instance Const [Char] Text where
  con s = Const (show s)

instance Const Int Number where
  con i = Const (show i)

instance Const Float Number where
  con i = Const (show i)

instance Const Bool Boolean where
  con False = Const "false"
  con True  = Const "true"


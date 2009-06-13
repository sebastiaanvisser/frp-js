{-# LANGUAGE FlexibleContexts, TypeFamilies, UndecidableInstances #-}
module Core.Raw where

import Control.Applicative
import Control.Monad.State
import Control.Monad
import Data.Foldable hiding (elem, mapM_, concatMap, concat)
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.Reify
import Data.Traversable hiding (mapM)
import qualified Core.Val as Ix
import qualified Data.Reify.Graph.CSE as CSE
import qualified Data.Map as Map
import qualified Data.IntSet as Set

data Val f =
    App f f
  | Arr f f
  | Comb [f]
  | Comp f f
  | Prim Ix.Type String String
  deriving (Eq, Ord, Show)

newtype Fix f = In { out :: f (Fix f) }

instance Show (f (Fix f)) => Show (Fix f) where
  show (In f) = "(" ++ show f ++ ")"

raw :: Ix.Val i -> Fix Val
raw (Ix.App f a)    = In $ App (raw f) (raw a)
raw (Ix.Arr a b)    = In $ Arr (raw a) (raw b)
raw (Ix.Comb fs)    = In $ Comb (map raw fs)
raw (Ix.Comp a b)   = In $ Comp (raw a) (raw b)
raw (Ix.Prim t d s) = In $ Prim t d s

combine :: [Ix.Val i] -> Fix Val
combine = In . Comb . map raw

fromIxValues :: [Ix.Val i] -> IO (Graph (DeRef (Fix Val)))
fromIxValues = fmap CSE.cse . reifyGraph . combine

instance Functor Val where
  fmap f (App a g)    = App (f a) (f g)
  fmap f (Arr a b)    = Arr (f a) (f b)
  fmap f (Comb fs)    = Comb (map f fs)
  fmap f (Comp a b)   = Comp (f a) (f b)
  fmap _ (Prim t d s) = Prim t d s

instance Foldable Val where
  fold (App a g)    = a `mappend` g
  fold (Arr a b)    = a `mappend` b
  fold (Comb fs)    = mconcat fs
  fold (Comp a b)   = a `mappend` b
  fold (Prim _ _ _) = mempty

instance Traversable Val where
  traverse f (App a g)    = App  <$> f a <*> f g
  traverse f (Arr a b)    = Arr  <$> f a <*> f b
  traverse f (Comb fs)    = Comb <$> traverse f fs
  traverse f (Comp a b)   = Comp <$> f a <*> f b
  traverse _ (Prim t d s) = pure (Prim t d s)

instance Traversable a => MuRef (Fix a) where
  type DeRef (Fix a) = a
  mapDeRef f = traverse f . out

-- Generic value traversal.

type Nodes = Map.Map Int (Val Int)

from :: Ord k => k -> Map.Map k a -> a
from f = fromMaybe (error "internal error in foldVal") . Map.lookup f

foldVal
  :: (Nodes -> Int -> Int -> [Int]                -> a)
  -> (Nodes -> Int -> Int -> Int                  -> a)
  -> (Nodes -> Int -> [Int]                       -> a)
  -> (Nodes -> Int -> Int -> Int                  -> a)
  -> (Nodes -> Int -> Ix.Type -> String -> String -> a)
  -> Graph Val
  -> [a]
foldVal f0 f1 f2 f3 f4 (Graph xs r) = evalState (folder (r, r `from` m)) Set.empty
  where
    m = Map.fromList xs
    folder (i, term) =
      let rec k = gets (Set.member k) <* modify (Set.insert k) >>=
                  mboolM (folder (k, k `from` m)) . not
          fn = fun m i term
          as = args m term
      in case term of
        App _ _    -> (\x y z -> x ++ concat y ++ z) <$> rec fn <*> mapM rec as <*> pure [f0 m i fn as]
        Arr a b    -> (\x y z -> x ++ y ++ z)        <$> rec a  <*> rec b       <*> pure [f1 m i a b]
        Comb fs    -> (\x y   -> concat x ++ y)      <$>            mapM rec fs <*> pure [f2 m i fs]
        Comp a b   -> (\x y z -> x ++ y ++ z)        <$> rec a  <*> rec b       <*> pure [f3 m i a b]
        Prim t d s ->                                                               pure [f4 m i t d s]

mboolM :: (Monad m, Monoid a) => m a -> Bool -> m a
mboolM a b = if b then a else return mempty

fun :: Nodes -> Int -> Val Int -> Int
fun m _ (App f _) = fun m f (f `from` m)
fun _ i _         = i

args :: Nodes -> Val Int -> [Int]
args m (App f a) = args m (f `from` m) ++ [a]
args _ _         = []


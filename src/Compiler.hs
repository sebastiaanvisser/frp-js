{-# LANGUAGE GADTs, RankNTypes #-}
module Compiler where

import Control.Monad.Identity
import Control.Monad.State
import Data.List
import Data.Map (Map)
import FRP
import qualified Data.Map as M

compile :: FRP () -> String
compile = compileNodes . runIdentity . flip execStateT []

compileNodes :: [Node a b] -> String
compileNodes nodes = 
  let prims = primitives nodes
      build = map (builder prims) (reverse nodes)
      decls = map (\(a, b) -> concat [b, " = ", a]) (M.toList prims)
  in intercalate "\n" (decls ++ build)

primitives :: [Node a b] -> Map String String
primitives =
    M.fromList
  . flip zip (map (("__x"++).show) [1::Int ..])
  . M.keys
  . M.filter (>1)
  . M.unionsWith (+)
  . map collect 
  . reverse

inc :: Maybe Int -> Maybe Int
inc Nothing  = Just 1
inc (Just i) = Just (i + 1)

collect :: Node a b -> Map String Int
collect (Stop  a)   = collect a
collect (Start a)   = collect a
collect (Comp a b)  = M.unionWith (+) (collect a) (collect b)
collect (Prim a)    = M.singleton a 1
collect (List xs)   = M.unionsWith (+) (map collect xs)
collect (App f s)   = M.unionWith (+) (collect f) (collect s)
collect (Const _)   = M.empty

builder :: Map String String -> Node a b -> String
builder e (Stop  a)   = builder e a
builder e (Start a)   = builder e a
builder e (Comp a b)  = builder e a ++ "(" ++ builder e b ++ ")"
builder e (Prim a)    = maybe a id (M.lookup a e)
builder e (List xs)   = "list(" ++ intercalate "," (map (builder e) xs) ++ ")"
builder e p@(App _ _) = fun e p ++ "(" ++ intercalate "," (args e p) ++ ")"
builder _ (Const b)   = "C(" ++ b ++ ")"

-- Flattening of curried function application.

args :: Map String String -> Node a b -> [String]
args e (App f a) = args e f ++ [builder e a]
args _ _         = []

fun :: Map String String -> Node a b -> String
fun e (App f _) = fun e f
fun e a         = builder e a


{-# LANGUAGE GADTs, RankNTypes #-}
module Core.Compiler where

import Control.Monad.Identity
import Control.Monad.State
import Data.List
import Data.Map (Map)
import Core.Val
import qualified Data.Map as M

compile :: FRP () -> String
compile = compileNodes . runIdentity . flip execStateT []

compileNodes :: [Val b] -> String
compileNodes nodes = 
  let prims = sharedPrimitives nodes
      build = map (builder prims) (reverse nodes)
      decls = map (\(a, b) -> concat [b, " = ", a]) (M.toList prims)
  in intercalate "\n" (decls ++ [""] ++ build)

sharedPrimitives :: [Val b] -> Map String String
sharedPrimitives =
    M.fromList
  . flip zip (map (("_"++).show) [0::Int ..])
  . M.keys
  . M.filterWithKey (\a _ -> a /= "/*cast*/")
  . M.unionsWith (+)
  . map collect 
  . reverse
  . (Prim list:)
  . (Prim comp:)

konst :: String -> String
konst k = "frp(" ++ k ++ ")"

list :: String
list = "combine(Array.concat)"

comp :: String
comp = "function(a,b)function()a(b.apply(undefined, arguments))"

collect :: Val b -> Map String Int
collect (App f s)  = M.unionWith  (+) (collect f) (collect s)
collect (Comb xs)  = M.unionsWith (+) (map collect xs)
collect (Comp a f) = M.unionWith  (+) (collect a) (collect f)
collect (Conn a b) = M.unionWith  (+) (collect a) (collect b)
collect (Const c)  = M.singleton (konst c) 1
collect (Prim a)   = M.singleton a 1

builder :: Map String String -> Val b -> String
builder e p@(App _ _) = fun e p ++ "(" ++ intercalate "," (args e p) ++ ")"
builder e (Comb xs)   = maybe "fail" id (M.lookup list e) ++ "(" ++ intercalate "," (map (builder e) xs) ++ ")"
builder e (Comp a f)  = maybe "fail" id (M.lookup comp e) ++ "(" ++ builder e a ++ "," ++ builder e f ++ ")"
builder e (Conn a b)  = builder e a ++ "(" ++ builder e b ++ ")"
builder e (Const c)   = let k = konst c in maybe k id (M.lookup k e)
builder e (Prim a)    = maybe a id (M.lookup a e)

-- Flattening of curried function application.

args :: Map String String -> Val b -> [String]
args e (App f a) = args e f ++ [builder e a]
args _ _         = []

fun :: Map String String -> Val b -> String
fun e (App f _) = fun e f
fun e a         = builder e a


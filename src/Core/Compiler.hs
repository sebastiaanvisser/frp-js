{-# LANGUAGE GADTs, RankNTypes #-}
module Core.Compiler where

{- TODO: cleanup `a bit'. -}

import Control.Applicative hiding (Const)
import Control.Monad.Identity
import Control.Monad.State
import Core.Val
import Data.List
import Data.Map (Map)
import Data.Ord
import qualified Data.Map as M

compile :: FRP () -> String
compile = runCompiler . runIdentity . flip execStateT []

runCompiler :: [Val a] -> String
runCompiler vs = 
    intercalate "\n"
  . filter (not . null)
  . map (showRule rev)
  . sortBy (comparing (fst . snd))
  . M.toList $ values
  where 
    swp (l, (i, n)) = (i, (l, n))
    rev = M.fromList . map swp . M.toList $ values
    values = snd . fst . runIdentity . flip runStateT 0
           . flip runStateT M.empty . mapM (compiler True) $ vs

type Id = Int

data Lang = D String | I String | A Id [Id] | C Id Id
  deriving (Eq, Ord)

instance Show Lang where
  show (D s)    = "frp(" ++ s ++ ")"
  show (I s)    = s
  show (A i xs) = "_" ++ show i ++ "(" ++ intercalate "," (map (\x -> "_" ++ show x) xs) ++ ")"
  show (C a b)  = "C(_" ++ show a ++ ",_" ++ show b ++ ")"

-- Common sub-expression elimination.

type CSE a = StateT (Map Lang (Id, (Int, Bool))) (StateT Id Identity) a

cse :: Bool -> Lang -> CSE Id
cse t lang =
  do item <- gets (M.lookup lang)
     case item of
       Nothing ->
         do f <- lift get
            lift (modify (+1))
            modify (M.insert lang (f, (1, t)))
            return f
       Just (i, (n, t')) ->
         do modify (M.insert lang (i, (n+1, t' || t)))
            return i

list :: String
list = "$(listify)"

compiler :: Bool -> Val a -> CSE Id
compiler t p@(App _ _)    = (A <$> fun p                       <*> args p                     ) >>= cse t
compiler t (Comb xs)      = (A <$> compiler False (Prim Fun "list" list) <*> mapM (compiler False) xs   ) >>= cse t
compiler t (Comp a f)     = (C <$> compiler False a            <*> compiler False f           ) >>= cse t
compiler t (Arr a b)      = (A <$> compiler False a            <*> (pure <$> compiler False b)) >>= cse t
compiler t (Prim Con _ a) = return (D a)                                                        >>= cse t
compiler t (Prim _   _ a) = return (I a)                                                        >>= cse t

fun :: Val a -> CSE Id
fun (App f _) = fun f
fun a         = compiler False a

args :: Val a -> CSE [Id]
args (App f as) = (\xs x -> xs ++ [x]) <$> args f <*> compiler False as
args _          = return []

use :: Id -> Map Id (Lang, (Int, Bool)) -> String
use k m = 
  case M.lookup k m of
    Just (a, (1, _)) -> showLang m a
    Just (_, (_, _)) -> '_':show k
    Nothing -> show "alert('compile error')"

showLang :: Map Id (Lang, (Int, Bool)) -> Lang -> String
showLang _ (D s)    = "frp(" ++ s ++ ")"
showLang _ (I s)    = s
showLang m (A i xs) = use i m ++ "(" ++ intercalate "," (map (\x -> use x m) xs) ++ ")"
showLang m (C a b)  = "C(" ++ use a m ++ "," ++ use b m ++ ")"

showRule :: Map Id (Lang, (Int, Bool)) -> (Lang, (Id, (Int, Bool))) -> String
showRule _ (I _,   (_, (1, _    ))) = ""
showRule _ (D _,   (_, (1, _    ))) = ""
showRule _ (C _ _, (_, (1, _    ))) = ""
showRule _ (_,     (_, (1, False))) = ""
showRule k (l,     (_, (1, _    ))) = showLang k l
showRule k (l,     (i, (_, _    ))) = intercalate "" ["_", show i, "=", showLang k l]


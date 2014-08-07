{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module AI where

import Control.Monad (when)
import Debug.Trace
import GHC.TypeLits
import qualified VM
import Control.Monad.State (State, modify, get, runState, put)
import Control.Applicative ((<$>), (<*>), pure)
import Data.List (intersperse)
import qualified Data.Set as S
import qualified Data.Map as M
import Prelude hiding ((||))

type Var = String
type Binding e = (Var, Closure e) 
type Closure n = (VM.LExpr, Env n)

type ClosureAnalysis a = M.Map (Closure a) (S.Set (Closure a)) 
data N = Z | S N

type CFA n = (S.Set (Closure (Depth n)), ClosureAnalysis (Depth n))

type family Depth n where
  Depth 0 = Z
  Depth n = S (Depth (n-1))

data Env n where 
  NoEnv :: Env Z
  Env :: [Binding n] -> Env (S n)

instance Eq (Env Z) where
  _ == _ = True

instance Eq (Env n) => Eq (Env (S n)) where
  (Env bs) == (Env bs') = bs == bs'

instance Ord (Env Z) where
  _ `compare` _ = EQ

instance Ord (Env n) => Ord (Env (S n)) where
  (Env bs) `compare` (Env bs') = bs `compare` bs'

class (Ord (Env e), Eq (Env e)) => EnvC e where
  empty :: Env e 
  restrict :: Env (S e) -> Env e
  relax :: Env e -> Env (S e)
  push :: Binding e -> Env (S e) -> Env (S e)
  fmap' :: ([Binding e] -> a) -> Env (S e) -> a

instance EnvC Z where
  empty = NoEnv
  restrict _ = NoEnv
  relax NoEnv = Env []
  push _ = id
  fmap' f (Env bs) = f bs

instance EnvC n => EnvC (S n) where
  empty = Env []
  restrict (Env bs) = Env [(v, (t, restrict e)) | (v,(t, e)) <- bs]
  relax (Env bs) = Env [(v, (t, relax e)) | (v,(t, e)) <- bs]
  push b (Env bs') = Env (b:bs')
  fmap' f (Env bs) = f bs

instance Show (Env n) where
  show NoEnv = "()"
  show (Env bs) = show bs

getEnv_ :: [Var] -> [Binding e] -> [Binding e]
getEnv_ [] _ = []
getEnv_ _ [] = []
getEnv_ (v:vs) ((v', b):bs) | v == v' = (v,b):getEnv_ vs bs
                            | otherwise = getEnv_ (v:vs) bs

cfa :: (Ord (Closure n), EnvC n) => VM.LExpr -> (S.Set (Closure (S n)), ClosureAnalysis (S n))
cfa t = cfa' t M.empty 
  where cfa' t m = case runState (eval' (S.singleton (t `close` empty)) S.empty) m of
                     (vs, m') -> if showsize m == m' then (vs, m') else cfa' t m'

showsize mu = trace (show (length $ M.keys mu, sum $ map (length . S.toList) $ M.elems mu)) mu

-- Takes a set of closures, and returns the set of values possible from that
-- closure. 
eval' :: EnvC n => S.Set (Closure (S n)) -> S.Set (Closure (S n)) -> State (ClosureAnalysis (S n)) (S.Set (Closure (S n)))
eval' cs seen = trace' ("eval' " ++ show cs) $ do
  sequence [eval_ c seen | c <- S.elems cs, not $ S.member c seen]
  getVals <$> get <*> pure cs <*> pure S.empty

getVals :: EnvC n => ClosureAnalysis (S n) -> S.Set (Closure (S n)) -> S.Set (Closure (S n)) ->  S.Set (Closure (S n))
getVals m cs visited = trace' ("getVal "++show cs) $ S.unions $ (vs:) $ S.toList $ S.map (gv . maybe S.empty id . flip M.lookup m) es
  where (vs, es) = S.partition (VM.isValue . fst) cs 
        gv cs' = getVals m (cs' S.\\ visited) (visited `S.union` cs)

-- Takes a set of closures and returns the set of un-evaluated closures that it
-- will evaluate to, possibly empty
getFurthest :: EnvC n => ClosureAnalysis (S n) -> S.Set (Closure (S n)) -> S.Set (Closure (S n)) ->  S.Set (Closure (S n))
getFurthest m cs visited = trace' ("getFurthest "++show cs) $ S.unions $ S.toList $ S.map gf (cs S.\\ visited)
  where gf c = case M.lookup c m of
                  Just cs' -> getFurthest m cs' (visited `S.union` S.singleton c)
                  Nothing -> S.singleton c

lu c m def = maybe def id $ M.lookup c m
trace' = flip const

-- Eval_ binds to its closure to the next equivalent closure found, and returns
-- the values it is equivalent to by calling eval' on that subsequent closure
-- when necessary
eval_ :: EnvC n => Closure (S n) -> S.Set (Closure (S n)) -> State (ClosureAnalysis (S n)) (S.Set (Closure (S n)))
eval_ c@(t, env) seen = trace' ("eval_ " ++ show c) $ get >>= \map -> case t of
  VM.Lam l v t -> set c $ S.singleton c
  VM.Lit l i -> set c $ S.singleton c
  VM.World l -> set c $ S.singleton c
  VM.Var l v -> do
    binders <- case fmap' (lookup v) (relax env) of 
        Nothing -> maybe (error$"Unbound var: " ++ v) id . M.lookup (t, Env []) <$> get
        Just c -> return $ S.singleton c
    newseen <- set c binders
    eval' binders (seen `S.union` newseen)
  VM.App l m n -> do
    mexprs <- S.toList <$> eval' (S.singleton (m `close` env)) (seen `S.union` S.singleton c)
    let applyLams = S.fromList [(b `close` push (v, (n `close` restrict env)) lamenv) | (VM.Lam l v b, lamenv) <- mexprs]
    sequence [set (v', Env []) (S.singleton (n `close` env)) | (VM.Lam l v b, lamenv) <- mexprs, v' <- binders v b]
    let applyLits = S.fromList [(VM.App l n t' `close` env) | (t'@(VM.Lit l i), _) <- mexprs]
    let applyWorlds = S.fromList [(VM.App l n t' `close` env) | (t'@(VM.World l), _) <- mexprs]
    newseen <- set c $ applyLams `S.union` applyLits `S.union` applyWorlds
    eval' (applyLams `S.union` applyLits `S.union` applyWorlds) (newseen `S.union` seen)

fvs :: VM.LExpr -> S.Set String
fvs e = case e of
  VM.Var l v -> S.singleton v
  VM.Lam l v b -> S.delete v $ fvs b
  VM.App l m n -> fvs m `S.union` fvs n
  _ -> S.empty

-- Scope takes free variables and returns the set of closures bound
scope :: S.Set String -> [Binding n] -> [Binding n]
scope vs env = case (S.size vs, env) of 
  (0, _) -> []
  (_, []) -> []
  (n, (b@(v,c):bs)) | S.member v vs -> b:scope (S.delete v vs) bs
                    | otherwise -> scope vs bs

close :: VM.LExpr -> Env n -> (Closure n)
close t NoEnv = (t, NoEnv)
close t (Env bs) = (t, Env $ scope (fvs t) bs)

binders :: String -> VM.LExpr -> [VM.LExpr]
binders v e = case e of
  VM.Var l v' -> if v == v' then [e] else []
  VM.Lam l v' e' -> if v == v' then [] else binders v e'
  VM.App l m n -> binders v m ++ binders v n
  _ -> []

set :: (Ord (Closure n), EnvC n) => Closure n -> S.Set (Closure n) -> State (ClosureAnalysis n) (S.Set (Closure n))
set c vs = modify (M.insertWith S.union c vs) >> return (S.singleton c)

ppca :: ClosureAnalysis n -> String
ppca m = concat.intersperse "\n".map pp.M.toList $ M.filterWithKey (\k v -> not . VM.isValue . fst $ k) $ m
  where pp ((t, e), ls) =  ppexpr t ++ ":" ++ ppset ls

ppexpr t = (\s->if length s == 20 then (s ++ "...") else s) $ take 30 $ show t
      
ppset ls = (concat . intersperse ", " . map (ppexpr . fst) . S.elems) ls 

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
{-# LANGUAGE TupleSections #-}

module State where

import Control.Monad (when)
import Debug.Trace
import GHC.TypeLits
import qualified VM
import Control.Applicative ((<$>), (<*>), pure)
import Data.List (intersperse)
import qualified Data.Set as S
import qualified Data.Map as M
import Prelude hiding ((||))
import Control.Monad.State (MonadState, execState, State, get, put, modify)

type Config n = (Closure n, M.Map (Closure n) (S.Set (Closure n)))
type Var = String
type Binding e = (Var, Closure e) 
type Closure n = (VM.LExpr, Env n)
type AIState n = State (M.Map (Config n) (S.Set (Config n)))
type ClosureAnalysis a = M.Map (Closure a) (S.Set (Closure a)) 
type AI n = M.Map (Config (Depth n)) (S.Set (Config (Depth n)))
type CFA n = ClosureAnalysis (Depth n)

data N = Z | S N

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
  show (Env bs) = unlines $ map (' ' :) $ lines $ concatMap (('\n' :) . show) bs

getEnv_ :: [Var] -> [Binding e] -> [Binding e]
getEnv_ [] _ = []
getEnv_ _ [] = []
getEnv_ (v:vs) ((v', b):bs) | v == v' = (v,b):getEnv_ vs bs
                            | otherwise = getEnv_ (v:vs) bs

combineConfigs :: EnvC n => M.Map (Config n) (S.Set (Config n)) -> ClosureAnalysis n
combineConfigs = undefined

cfa :: EnvC n => VM.LExpr -> ClosureAnalysis (S n)
cfa = combineConfigs . states

states :: EnvC n => VM.LExpr -> M.Map (Config (S n)) (S.Set (Config (S n)))
states t = execState (eval ((t, empty), M.empty)) M.empty

trace' = trace

eval :: EnvC n => Config (S n) -> AIState (S n) ()
eval s@(c@(t, env), mu) = trace' (ppstate s) $ get >>= \ai -> trace (show $ M.size ai) $ if elem s $ M.keys ai then return () else case t of
  VM.Var l v -> case fmap' (lookup v) (relax env) of
    Nothing -> cont s $ S.map (,mu) $ lu c mu
    Just c' -> cont s $ S.singleton (c',mu)
  VM.App l m n -> do
    cont s S.empty
    eval ((m, env), mu) 
    ai' <- get 
    let vals = getvals ai' [((m, env), mu)] S.empty
    let bodies = [((b, push (v,(n `close` restrict env)) env'), 
                  M.unionWith S.union (M.fromList [((v', Env []), (S.singleton (n,env))) | v' <- binders v b]) mu')
                  | ((VM.Lam l v b,env'),mu') <- vals]
    cont s $ S.fromList bodies
    mapM_ eval bodies
  _ -> cont s $ S.singleton s
  where cont s ss = modify (M.insertWith S.union s ss) >> sequence_ [eval s | s <- S.toList ss] 

getvals m [] vis = []
getvals m (s@((t,env), mu):ss) vis | VM.isValue t = s : getvals m ss (S.insert s vis)
getvals m (s:ss) vis = getvals m (maybe ss ((ss ++) . (S.elems) . (S.\\ vis)) $ M.lookup s m) (S.insert s vis)

trc a = trace (show a) a

lu c@(t,env) m = maybe (error $ "Undbound var: " ++  VM.showlabeled t) id $ M.lookup c m

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

ppstates m = concatMap f $ M.toList m
  where f (s, ss) = ppstate s  ++ " -> \n" ++ concatMap (("\t" ++) . (++ "\n") . ppstate) (S.toList ss)

ppstate (c,mu) = show c ++ " * " ++ ppca mu  

ppca :: ClosureAnalysis n -> String
ppca m = concat.intersperse "\n    ".map pp.M.toList $ M.filterWithKey (\k v -> not . VM.isValue . fst $ k) $ m
  where pp ((t, e), ls) =  ppexpr t ++ ":" ++ ppset ls

ppexpr t = (\s->if length s == 20 then (s ++ "...") else s) $ take 30 $ show t
       
ppset ls = (concat . intersperse ", " . map (ppexpr . fst) . S.elems) ls 

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

import GHC.TypeLits
import Debug.Trace
import qualified VM
import Control.Monad.State (State, modify, get, runState)
import Control.Applicative ((<$>), (<*>), pure)
import Data.List (intersperse)
import qualified Data.Set as S
import qualified Data.Map as M

type Var = String
type Binding e = (Var, Closure e)
type Closure e = (VM.LExpr, Env e)
type ClosureAnalysis a = M.Map (Closure a) (S.Set (Closure a)) 
data N = Z | S N

type CFA n = ClosureAnalysis (Depth n)

type family Depth n where
  Depth 0 = Z
  Depth n = S (Depth (n-1))

data Env n where 
  NoEnv :: Env Z
  Env :: [Binding n] -> Env (S n)

instance Ord (Env n) where
  compare NoEnv NoEnv = EQ
  compare (Env bs) (Env cs) = compare bs cs

instance Eq (Env n) where
  NoEnv == NoEnv = True
  Env bs == Env cs = cs == bs

class EnvC e where
  empty :: Env e 
  restrict :: Env (S e) -> Env e
  relax :: Env e -> Env (S e)
  push :: Binding e -> Env (S e) -> Env (S e)
  fmap' :: ([Binding e] -> a) -> Env (S e) -> a

instance EnvC Z where
  empty = NoEnv
  restrict x = NoEnv
  relax NoEnv = Env []
  push x = id
  fmap' f (Env bs) = f bs

instance EnvC n => EnvC (S n) where
  empty = Env []
  restrict (Env bs) = Env [(v, (t, restrict e)) | (v,(t,e)) <- bs]
  relax (Env bs) = Env [(v, (t, relax e)) | (v,(t,e)) <- bs]
  push b (Env bs') = Env (b:bs')
  fmap' f (Env bs) = f bs

instance Show (Env n) where
  show NoEnv = "()"
  show (Env bs) = show bs

getEnv_ :: [Var] -> [Binding e] -> [Binding e]
getEnv_ [] bs = []
getEnv_ vs [] = []
getEnv_ (v:vs) ((v', b):bs) | v == v' = (v,b):getEnv_ vs bs
                            | otherwise = getEnv_ (v:vs) bs

eval :: EnvC n => VM.LExpr -> ClosureAnalysis (S n)
eval t = snd $ runState (eval' (S.singleton (t, empty))) M.empty

-- Takes a set of closures, and returns the set of values possible from that
-- closure.
eval' :: EnvC n => S.Set (Closure (S n)) -> State (ClosureAnalysis (S n)) (S.Set (Closure (S n)))
eval' cs = do
  case S.elems cs of
    [] -> return S.empty
    c:cs' -> eval' =<< S.union (S.fromList cs') <$> eval_ c
  getVals <$> get <*> pure cs <*> pure S.empty

getVals :: ClosureAnalysis (S n) -> S.Set (Closure (S n)) -> S.Set (Closure (S n)) ->  S.Set (Closure (S n))
getVals m cs visited = S.unions $ (vs:) $ S.toList $ S.map (maybe (error "EMPTY!") id . flip M.lookup m) es
  where (vs, es) = S.partition (VM.isValue . fst) cs 
        gv cs' = getVals m (cs' S.\\ visited) (visited `S.union` cs)

-- Eval_ binds to its closure to the next equivalent closure found, and returns
-- the values it is equivalent to by calling eval' on that subsequent closure
-- when necessary
eval_ :: EnvC n => Closure (S n) -> State (ClosureAnalysis (S n)) (S.Set (Closure (S n)))
eval_ c@(t, env) = get >>= \map -> case M.lookup c map of
  Just cs -> eval' $ S.filter (\c'@(t,e) -> c' /= c && not (VM.isValue t)) cs
  Nothing -> case t of
    VM.Lam l v t -> set c $ S.singleton c
    VM.Lit l i -> set c $ S.singleton c
    VM.World l -> set c $ S.singleton c
    VM.Var l v -> do
      binders <- case fmap' (lookup v) (relax env) of 
          Nothing -> maybe (error$"Unbound var: " ++ v) id . M.lookup (t, Env []) <$> get
          Just c -> return $ S.singleton c
      set c binders
      eval' binders
    VM.App l m n -> do
      mexprs <- S.toList <$> eval' (S.singleton (m, env))
      let applyLams = S.fromList [(b, push (v, (n, restrict env)) lamenv) | (VM.Lam l v b, lamenv) <- mexprs]
      sequence [set (v', Env []) (S.singleton (n, env)) | (VM.Lam l v b, lamenv) <- mexprs, v' <- binders v b]
      set c applyLams
      lamVals <- eval' applyLams
      litVals <- S.unions <$> sequence [eval' $ S.singleton (VM.App l n t', env) | (t'@(VM.Lit l i), _) <- mexprs]
      worldVals <- S.unions <$> sequence [eval' $ S.singleton (VM.App l n t', env) | (t'@(VM.World l), _) <- mexprs]
      return $ litVals `S.union` lamVals `S.union` worldVals

binders :: String -> VM.LExpr -> [VM.LExpr]
binders v e = case e of
  VM.Var l v' -> if v == v' then [e] else []
  VM.Lam l v' e' -> if v == v' then [] else binders v e'
  VM.App l m n -> binders v m ++ binders v n
  _ -> []

set :: EnvC n => Closure n -> S.Set (Closure n) -> State (ClosureAnalysis n) (S.Set (Closure n))
set c vs = modify (M.insertWith S.union c vs) >> return vs

ppca :: ClosureAnalysis n -> String
ppca m = concat.intersperse "\n".map pp.M.toList.M.map S.toList $ M.filterWithKey (\k v -> not . VM.isValue . fst $ k) $ m
  where pp (v, ls) = show v ++ ":" ++ (concat . intersperse ", " . map show) ls
        ppexpr t = (\s->if length s == 20 then (s ++ "...") else s) $ take 20 $ show t

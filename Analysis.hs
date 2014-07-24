module Analysis where
import qualified LC
import qualified Data.IntMap as M
import qualified Data.IntSet as S
import qualified Data.Map as Map
import qualified Data.Set as Set
import Debug.Trace (trace)
import Data.Maybe (maybe, fromJust)
import Data.List (intersperse)
import Control.Monad.State
import Control.Applicative
import IO
import qualified Data.GraphViz as GV
import VM
import Data.Array

-- Types
type Analysis = M.IntMap
type Labels = S.IntSet
type ValueAnalysis = Analysis S.IntSet -- Set of values possible at a location
type VariableAnalysis = Analysis S.IntSet -- Set of variables that can be called
type FreeVariableAnalysis = Analysis (Set.Set String) -- Free variables at a location
type CheckList = Analysis Bool -- Free variables at a location

-- Binds variable to lambda
binders :: LExpr -> Analysis Labels
binders e = bind e Map.empty
  where bind e env = case e of 
          Var l v -> M.singleton (fromJust $ Map.lookup v env) (S.singleton l)
          Lam l v e -> bind e $ Map.insert v l env 
          App l m n -> M.unionWith S.union (bind m env) (bind n env)
          _ -> M.empty           

-- Body of lambda expressions
bodies :: LExpr -> Analysis Label
bodies (Lam l v e) = M.insert l (getLabel e) $ bodies e 
bodies (App l m n) = M.union (bodies m) (bodies n)
bodies _ = M.empty

-- Closure analysis
ca :: LExpr -> ValueAnalysis
ca e = ca'' M.empty where 
  ca'' :: ValueAnalysis -> ValueAnalysis 
  ca'' mu = if mu == mu' then mu else ca'' mu' where (_, mu') = ca' e (S.empty, mu)
  ca' :: LExpr -> (S.IntSet, ValueAnalysis) -> (S.IntSet, ValueAnalysis)
  ca' e (ch,mu) | (S.member (getLabel e) ch) = (ch,mu)
  ca' (Var l v) (ch,mu) = let 
    params = [(l',fjlu l' exps) | l' <- S.toList $ lu l mu]
    (ch', mu') = foldr ca' (S.insert l ch,mu) (map snd params)
    in (ch', M.fromListWith S.union [(l, lu l' mu') | (l',_) <- params] `union` mu')
  ca' (Lam l v e) (ch,mu) = ca' e (S.insert l ch, M.insert l (S.singleton l) mu)
  ca' (App l e1 e2) (ch, mu) = ca' e1 (S.insert l ch, union mu $ M.fromListWith S.union $ 
    [(l, lu (fjlu lam bods) mu) | lam <- S.toList $ lu (getLabel e1) mu, islam lam]
    ++ [(v, S.singleton $ getLabel e2) | v <- S.toList $ S.unions $ map (flip lu bins) $ filter islam $ S.toList $ lu (getLabel e1) mu])
  ca' e (ch,mu) = (S.insert (getLabel e) ch, M.insertWith S.union l (S.singleton l) mu) where l = getLabel e
  bins = (binders e)
  bods = (bodies e)
  exps = (expr e)
  islam l = case fjlu l exps of Lam _ _ _ -> True; _ -> False
  isval l = isValue $ fjlu l exps

-- Free variable analysis
fv :: LExpr -> FreeVariableAnalysis
fv (Var l v) = M.singleton l (Set.singleton v)
fv (Lam l v e) = let fve = fv e in M.insert l (Set.filter (/= v) $ fjlu (getLabel e) fve) fve
fv (App l m n) = let fvmn = fv m `M.union` fv n in M.insert l (fjlu (getLabel m) fvmn `Set.union` fjlu (getLabel n) fvmn) fvmn
fv a = M.empty

-- Utility functions
union :: M.IntMap S.IntSet -> M.IntMap S.IntSet -> M.IntMap S.IntSet
union = M.unionWith S.union

lu :: M.Key -> M.IntMap S.IntSet -> S.IntSet
lu k m = maybe S.empty id $ M.lookup k m  

fjlu :: Int -> M.IntMap a -> a
fjlu v = maybe (error $ "couldn't find: " ++ show v) id . M.lookup v

ppca :: ValueAnalysis -> String
ppca m = concat.intersperse "\n".map pp.M.toList.M.map S.toList $ m
  where pp (v, ls) = show v ++ ":" ++ (concat . intersperse ", " . map show) ls


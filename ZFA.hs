module MZFA where
import qualified LC
import Utils
import qualified Data.IntMap as M
import qualified Data.IntSet as S
import qualified Data.Map as Map
import qualified Data.Set as Set
import Debug.Trace (trace)
import Data.Maybe (maybe, fromJust, maybeToList, fromMaybe)
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
type ValueAnalysis = Analysis (Maybe S.IntSet) -- Maybe a set, where Nothing = Anything; 
type VariableAnalysis = Analysis S.IntSet -- Set of variables that can be called
type FreeVariableAnalysis = Analysis S.IntSet -- Free variables' binding locations at a location
type CheckList = Analysis Bool -- Free variables at a location

-- Binds lambda to its variables
binders :: LExpr -> Analysis Labels
binders e = bind e [] 
  where bind e env = case e of 
          Var l v -> M.singleton (maybe (error$"unbound variable: " ++ v) id (lookup v env)) (S.singleton l)
          Lam l v e -> bind e $ (v,l):env 
          App l m n -> M.unionWith S.union (bind m env) (bind n env)
          Op l o -> case o of
            LC.Le -> bool 
            LC.Ge -> bool 
            LC.Lt -> bool 
            LC.Gt -> bool 
            LC.Eq -> bool 
            LC.Neq -> bool 
            LC.Syscall n -> binders (returnval l)
            LC.Read n -> binders (returnval l)
            _ -> M.empty
            where bool = M.fromList [(v, S.singleton l) | (var, v) <- take 2 $ drop 2 env]
          _ -> M.empty           

-- Body of lambda expressions
bodies :: LExpr -> Analysis Label
bodies (Lam l v e) = M.insert l (getLabel e) $ bodies e 
bodies (App l m n) = M.union (bodies m) (bodies n)
bodies _ = M.empty

size = sum . concatMap maybeToList . map (S.size <$>) . M.elems

munion x y = S.union <$> x <*> y

restrict :: Int -> ValueAnalysis -> ValueAnalysis
restrict i = M.map (size =<<)
  where size s = if S.size s > i then Nothing else Just s

-- Closure analysis
ca :: Int -> LExpr -> ValueAnalysis
ca i e = ca'' M.empty where 
  ca'' :: ValueAnalysis -> ValueAnalysis 
  ca'' mu = if trace (show $ size mu)
               mu == mu' then mu else ca'' (restrict i mu') where (_, mu') = ca' e (S.empty, mu)
  ca' :: LExpr -> (S.IntSet, ValueAnalysis) -> (S.IntSet, ValueAnalysis)
  ca' e (ch,mu) | (S.member (getLabel e) ch) = (ch,mu)
  ca' (Var l v) (ch,mu) = case lu l mu of
    Just vs -> let  
      params = [(l',fjlu l' exps) | l' <- S.elems vs]
      (ch', mu') = foldr ca' (S.insert l ch,mu) (map snd params)
      in (ch', M.fromListWith munion [(l, lu l' mu') | (l',_) <- params] `union` mu')
    Nothing -> (S.insert l ch, M.insertWith munion l Nothing mu)
  ca' (Lam l v e) (ch,mu) = ca' e (S.insert l ch, M.insert l (Just$S.singleton l) mu)
  ca' (App l e1 e2) (ch, mu) = case lu (getLabel e1) mu of
    Nothing -> ca' e2 (ca' e1 (S.insert l ch, M.insertWith munion l Nothing mu))
    Just lazyvals -> ca' e1 (S.insert l ch', M.unionsWith munion [mu', lazyBodies, lazyBinders]) where
      (strictch, strictmu) = ca' e2 (S.insert l ch, M.unionsWith munion [mu, strictBodies, strictBinders])
      lits = filter islit $ S.elems lazyvals 
      strictLams = [lam | lam <- maybe [] S.toList $ lu (getLabel e2) mu, islam lam]
      strictBodies = M.fromListWith munion [(l, lu (lam + 1) mu) | lam <- strictLams ]
      strictBinders = M.fromListWith munion 
                        [(v, Just $ S.singleton lit) | 
                         lit <- lits,
                         lam <- strictLams,
                         v <- S.toList $ fromMaybe S.empty $ M.lookup lam bins]
      lazyLams = [lam | lam <- S.elems lazyvals, islam lam] 
      lazyBodies = M.fromListWith munion [(l, lu (lam + 1) mu) | lam <- lazyLams]
      lazyBinders = M.fromListWith munion 
                      [(v, Just $ S.singleton $ getLabel e2) | 
                       lam <- lazyLams,
                       v <- maybe [] S.toList $ M.lookup lam bins]
      (ch', mu') = if null lits 
        then (ch, mu) 
        else case lu (getLabel e2) mu of
          Nothing -> ca' e2 (ca' e1 (S.insert l ch, M.insertWith munion l Nothing mu))
          Just cs -> (strictch, strictmu)
  ca' (Lit l i) (ch,mu) = (S.insert l ch, M.insert l (Just $ S.singleton l) mu)
  ca' (World l) (ch,mu) = (S.insert l ch, M.insert l (Just $ S.singleton l) mu)
  ca' (Op l o) (ch,mu) = case o of 
    LC.Syscall n -> ca' (returnval l) (ch', mu')
    LC.Write w -> ca' (returnval l) (ch', mu')
    LC.Read w -> ca' (returnval l) (ch', mu')
    LC.Add -> lit
    LC.Sub -> lit    
    LC.Mul -> lit
    LC.Div -> lit
    LC.Mod -> lit
    LC.Le -> bool
    LC.Ge -> bool 
    LC.Lt -> bool 
    LC.Gt -> bool 
    LC.Eq -> bool 
    LC.Neq -> bool 
    where (ch', mu') = (S.insert l ch, M.insert l (Just $ S.singleton (l+1)) mu)
          lit = ca' (Lit (l+1) Nothing) (ch', mu')
          bool = case lu l mu of 
            Nothing -> (ch', mu)
            Just vs -> (boolch, M.fromListWith munion [(l, lu l' boolmu) | (l',_) <- params] `union` boolmu)
            where params = [(l',fjlu l' exps) | l' <- maybe [] S.toList $ lu l mu]
                  (boolch, boolmu) = foldr ca' (ch',mu) (map snd params)
  bins = binders e
  bods = bodies e
  exps = expr e
  fvs = fv e
  islam l = case fjlu l exps of Lam _ _ _ -> True; _ -> False
  islit l = case fjlu l exps of Lit _ _ -> True; World _ -> True; _ -> False
  sizeof mu = sum $ M.elems $ M.map S.size mu

-- Free variable analysis
fv :: LExpr -> Analysis [Int]
fv = fv' [] where
  fv' bs (Var l v) = case lookup v bs of
    Nothing -> error $ "unbound var: "++v
    Just l' -> M.singleton l $ [l']
  fv' bs (Lam l v e) = M.insert l (filter (/= l) $ fjlu (getLabel e) fve) fve
    where fve = fv' ((v,l):bs) e 
  fv' bs (App l m n) = M.insert l (fjlu (getLabel m) fvmn ++ 
                                  (fjlu (getLabel n) fvmn)) fvmn
    where fvmn = fv' bs m `M.union` fv' bs n 
  fv' bs (Op l o) = case o of
    LC.Syscall n -> frees $ n + 3 
    LC.Write w ->  frees 3
    LC.Read w -> frees 2
    LC.Add -> frees 2
    LC.Sub -> frees 2
    LC.Mul -> frees 2
    LC.Div -> frees 2
    LC.Mod -> frees 2
    LC.Le -> frees 4 
    LC.Ge -> frees 4
    LC.Lt -> frees 4
    LC.Gt -> frees 4
    LC.Eq -> frees 4
    LC.Neq -> frees 4
    where frees n = M.singleton l $ map snd $ take n bs
  fv' bs (World l) = M.empty
  fv' bs (Lit l i) = M.empty

-- Utility functions
union :: ValueAnalysis -> ValueAnalysis -> ValueAnalysis
union = M.unionWith munion

lu :: M.Key -> ValueAnalysis -> Maybe (S.IntSet)
lu k m = fromMaybe (Just S.empty) $ M.lookup k m  

fjlu :: Int -> M.IntMap a -> a
fjlu v = maybe (error $ "couldn't find: " ++ show v) id . M.lookup v

ppca :: LExpr -> ValueAnalysis -> String
ppca e m = concat.intersperse "\n".map pp.M.toList.M.map (fmap (filter valuefilter.S.toList)).M.filterWithKey keyfilter $ m
  where pp (v, ls) = case ls of 
          Just ls -> ppexpr v ++ ":" ++ (concat . intersperse ", " . map ppexpr) ls
          Nothing -> ppexpr v ++ ":" ++ "⊤"
        ppexpr v = (\s->if length s == 20 then (s ++ "...") else s) $ take 20 $ show $ maybe (Var v "?") id (M.lookup v exps)
        exps = expr e
        keyfilter k v = not $ isValue $ fjlu k exps 
        valuefilter v = isValue $ fjlu v exps


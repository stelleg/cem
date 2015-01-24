{-#LANGUAGE OverloadedStrings#-}

module Analysis where
import GHC.TypeLits
import Data.Text.Lazy (pack)
import qualified LC
import qualified Data.Map as M
import qualified Data.Set as S
import Debug.Trace (trace)
import Data.Maybe (maybe, fromJust)
import Data.List (intersperse)
import Control.Monad.State
import Control.Applicative ((<$>), (<*>))
import IO
import qualified Data.GraphViz as GV
import VM (LExpr (..), returnval, isValue, getLabel, showGraph, process)
import Data.Array
import Data.List (sortBy)
import Data.GraphViz.Types.Monadic
import Data.GraphViz.Attributes.Complete hiding (Lt, label, Label)
import Data.GraphViz.Attributes
import Data.GraphViz.Commands

-- Types
data Closure = Closure {term :: LExpr, env :: [Binding]} deriving (Eq, Ord)
type Var = String
type Label = Int
type Binding = (String, Closure)
type Analysis = M.Map LExpr
type ClosureAnalysis = M.Map Closure (S.Set Closure) 
type CFA = (S.Set Closure, ClosureAnalysis)

instance Show Closure where
  show (Closure t e) = "<" ++ show t ++ " | " ++ show e ++ ">"

restrict :: Int -> Closure -> Closure
restrict 0 (Closure t e) = (if length e > 0 
  then {-trace ("Restricting!:" ++ show (Closure t e)) -} id
  else id) Closure t []
restrict i (Closure t e) = Closure t [(v, restrict (i-1) c) | (v,c) <- e]

-- Binds lambda to its variables
binders :: LExpr -> M.Map Label [LExpr]
binders e = bind e [] 
  where bind e env = trace' ("binders: " ++ show e) $ case e of 
          Var l v -> M.singleton (maybe (error$"unbound variable: " ++ v) id (lookup v env)) [e]
          Lam l v e' -> bind e' $ (v,l):env 
          App l m n -> M.unionWith (++) (bind m env) (bind n env)
          Op l o -> case o of
            LC.Le -> bool 
            LC.Ge -> bool 
            LC.Lt -> bool 
            LC.Gt -> bool 
            LC.Eq -> bool 
            LC.Neq -> bool 
            LC.Syscall n -> binders (returnval $ getLabel e)
            LC.Read n -> binders (returnval $ getLabel e)
            _ -> M.empty
            where bool = M.fromList [(v, [e]) | (var, v) <- take 2 $ drop 2 env]
          _ -> M.empty           

size :: M.Map a (S.Set b) -> Int
size = sum . map S.size . M.elems

ca' ca t = getVals ca (S.singleton (t `Closure` [])) S.empty

summarize ca = foldr f M.empty $ M.keys ca
  where f c m = M.insertWith S.union (term c) (getVals ca (S.singleton c) S.empty) m

sizes :: ClosureAnalysis -> [(LExpr, ((Int, Int), (Int, Int)))]
sizes ca = sortBy (\(a,b) (c,d) -> compare b d) $ M.toList $ foldr f M.empty $ M.keys ca
  where f c m = M.insertWith g (term c) (val c) m
        g ((a,b),(c,d)) ((a',b'),(c',d')) = ((a+a',b+b'),(c+c',d+d'))
        val c = case (env c) of
          [] -> ((1,S.size $ fjlu c ca), (0,0))
          env -> ((0,0), (1,S.size $ fjlu c ca))

graphCFA :: ClosureAnalysis -> IO ()
graphCFA cfa = showGraph "cfa.dot" dot where 
  dot = digraph (Str "CFA") $ do
    graphAttrs [textLabel "CFA"]
    let nodes = M.toList cfa 
    sequence [node (getLabel $ term c) [shape Record, textLabel (pack$process$take 15$show$term c)] | (c, cs) <- nodes]
    sequence [edge (getLabel $ term c) (getLabel $ term c') [] | (c,cs) <- nodes, c' <- S.toList cs] 

ca :: (ClosureAnalysis -> IO ()) -> Int -> LExpr -> IO ClosureAnalysis
ca tr dep e = findFixed M.empty where
  findFixed :: ClosureAnalysis -> IO ClosureAnalysis
  findFixed mu = tr mu >> if size mu == size mu' 
    then return mu 
    else findFixed mu' 
    where (_, mu') = eval (e `close` []) (S.empty, mu)
  eval :: Closure -> CFA -> CFA
  eval c (ch, mu) | S.member c ch = (ch, mu)
  eval c (ch, mu) = trace' ("eval: " ++ show c) $ case term c of
    Var l v -> next (ch,mu) c $ maybe (fjlue ("unbound var in global store: " ++ show c) (term c `close` []) mu) S.singleton $ lookup v $ env c
    App l m n -> next (nextch, nextmu) c $ S.fromList nextbods
      where (nextch, nextmu, nextbods) = if null mlits 
              then (mch, mmu `union` mvarmu, mbods)
              else (S.union mch nch, M.unionsWith S.union [mmu, nmu, mvarmu, nvarmu], nbods ++ mbods)
            (mch, mmu) = eval (m `close` env c) (S.insert c ch, mu)
            mvals = S.elems $ getVals mu (S.singleton (m `close` env c)) S.empty
            mbods = [restrict dep (b `close` (:) (v, (n `close` (env c))) env') | Closure (Lam l v b) env' <- mvals ]
            mvars = [M.fromList [((v' `close` []), S.singleton (n `close` env c)) | v' <- lu l bins] | Closure (Lam l v b) env' <- mvals]
            mvarmu = M.unionsWith S.union mvars
            mlits = filter islit $ mvals
            (nch, nmu) = eval (n `close` env c) (S.insert c ch, mu)
            nlams = [c | c@(Closure (Lam l v b) env') <- S.elems $ getVals nmu (S.singleton (n `close` env c)) S.empty]
            nbods = [restrict dep (b `close` (:) (v, l') env') | Closure (Lam l v b) env' <- nlams, l' <- mlits]
            nvars = [M.fromList [((v' `close` []), S.singleton l') | v' <- lu l bins] | Closure (Lam l v b) env' <- nlams, l' <- mlits]
            nvarmu = M.unionsWith S.union nvars 
    Op l o -> case o of 
      LC.Syscall n -> next (ch,mu) c $ S.singleton (returnval l `Closure` [])
      LC.Write w -> next (ch,mu) c $ S.singleton (returnval l `Closure` [])
      LC.Read w -> next (ch,mu) c $ S.singleton (returnval l `Closure` [])
      LC.Add -> lit (+)
      LC.Sub -> lit (-)
      LC.Mul -> lit (*)
      LC.Div -> lit div
      LC.Mod -> lit mod
      LC.Le -> bool (<=)
      LC.Ge -> bool (>=)
      LC.Lt -> bool (<)
      LC.Gt -> bool (>)
      LC.Eq -> bool (==)
      LC.Neq -> bool (/=)
      where lit op = next (ch,mu) c $ case env c of
              [a,b] -> S.singleton $ Closure (Lit (l+1) Nothing) []
              [(_, Closure (Lit _ b) _), (_, Closure (Lit _ a) _)] -> S.singleton $ Closure (Lit (l+1) $ op <$> a <*> b) []
              _ -> trace "Warning: possible type error on primop" $ S.singleton $ Closure (Lit (l+1) Nothing) []
            bool op = next (ch,mu) c $ case env c of 
              [b,a,(_,cf),(_,ct)] -> S.fromList [ct,cf]
              [(_, Closure (Lit _ b) _), (_, Closure (Lit _ a) _), (_,cf),(_,ct)] -> case op <$> a <*> b of 
                Nothing -> S.fromList [ct, cf]
                Just b -> S.singleton $ if b then ct else cf
              _ -> trace "Warning: possible type error on primop" $ fjlu (term c `close` []) mu
    _ -> next (ch,mu) c $ S.singleton c
    where next (ch,mu) c s = S.foldr eval (S.insert c ch, M.insert c s mu) s
          islit c = case term c of World _ -> True; Lit _ _ -> True; _ -> False
  bins = binders e
  fvs = fv e
  close :: VM.LExpr -> [Binding] -> Closure
  close t bs = Closure t $ scope (S.fromList (fjlu (getLabel t) fvs)) bs

scope :: S.Set String -> [Binding] -> [Binding]
scope vs env = case (S.size vs, env) of 
  (0, _) -> []
  (_, []) -> []
  (n, (b@(v,c):bs)) | S.member v vs -> b:scope (S.delete v vs) bs
                    | otherwise -> scope vs bs

union :: (Ord a, Ord b) => M.Map a (S.Set b) -> M.Map a (S.Set b) -> M.Map a (S.Set b)
union = M.unionWith S.union

trace' = flip const

getVals :: ClosureAnalysis -> S.Set Closure -> S.Set Closure ->  S.Set Closure
getVals m cs visited = trace' ("getVal "++show cs) $ S.unions $ (vs:) $ S.toList $ S.map (gv . maybe S.empty id . flip M.lookup m) es
  where (vs, es) = S.partition (isValue . term) cs 
        gv cs' = getVals m (cs' S.\\ visited) (visited `S.union` cs)

ppca :: ClosureAnalysis -> String
ppca m = concat.intersperse "\n".map pp.M.toList $ M.filterWithKey (\k v -> not . VM.isValue . term $ k) $ m
  where pp (c, ls) =  "<" ++ ppexpr (term c) ++ "," ++ show (env c) ++ ">" ++ ": " ++ ppset ls

ppta :: M.Map LExpr (S.Set Closure) -> String
ppta m = concat.intersperse "\n".map pp.M.toList $ M.filterWithKey (\k v -> not . VM.isValue $ k) $ m
  where pp (t, ls) =  ppexpr t ++ ":" ++ ppset ls

ppexpr t = (\s->if length s == 30 then (s ++ "...") else s) $ take 30 $ show t
       
ppset ls = (concat . intersperse ", " . map (ppexpr . term) . S.elems) ls 
              
-- Free variable analysis
fv :: LExpr -> M.Map Int [String]
fv = fv' [] where
  fv' bs (Var l v) = M.singleton l [v]
  fv' bs (Lam l v e) = M.insert l (filter (/= v) $ fjlu (getLabel e) fve) fve
    where fve = fv' (v:bs) e 
  fv' bs (App l m n) = M.insert l (fjlu (getLabel m) fvmn ++ 
                                  (fjlu (getLabel n) fvmn)) fvmn
    where fvmn = fv' bs m `M.union` fv' bs n
  fv' bs (Op l o) = case o of
    LC.Syscall n -> frees (n + 3) `M.union` (fv $ returnval l)
    LC.Write w ->  frees 3 `M.union` (fv $ returnval l)
    LC.Read w -> frees 2 `M.union` (fv $ returnval l)
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
    where frees n = M.singleton l $ take n bs
  fv' bs (World l) = M.singleton l $ []
  fv' bs (Lit l i) = M.singleton l $ []

fjlue err l m = maybe (error err) id $ M.lookup l m
fjlu l m = maybe (error $ "fjlu " ++ show l) id $ M.lookup l m
lu k m = maybe [] id $ M.lookup k m  


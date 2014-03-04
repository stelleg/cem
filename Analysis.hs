module Analysis where
import qualified LC
import qualified Data.Map as M
import qualified Data.Set as S
import Debug.Trace (trace)
import Data.Maybe (maybe, fromJust)
import Data.List (intersperse)
import Control.Monad.State
import Control.Applicative
import IO
import qualified Data.GraphViz as GV

type Label = String
type Index = Int
data Expr = Lam Label Expr 
          | Var Label 
          | App Index Expr Expr 
          | Lit LC.Literal 
          | ULit Index 
          | UBool Label Label

data MetaVar = V Label | L Label | A Index | C LC.Literal | UL Index | UB Label Label deriving (Ord, Eq)
type Analysis a = M.Map MetaVar a
type Bodies = Analysis MetaVar
data Value = LV Label | CV LC.Literal | ULV Index | Unknown deriving (Eq, Ord)
type ClosureAnalysis = Analysis (S.Set Value)
type FreeVariableAnalysis = Analysis (S.Set Label)
data OpType = OpBinLit | OpBinBool | OpLitWorld | OpWorldLit

labeled :: LC.Expr Label Label -> Expr
labeled e = evalState (label e) (0,M.empty)

label :: LC.Expr Label Label -> State (Int, M.Map Label Label) Expr 
label (LC.Var v) = Var . fjlu v . snd <$> get
label (LC.Lam v e) = do modify (fmap $ M.insertWith (\x y->y ++ "'") v v)
                        Lam . fjlu v . snd <$> get <*> label e 
label (LC.App m n) = do modify (\(i,ls) -> (i+1,ls))
                        App . fst <$> get <*> label m <*> label n
label (LC.Lit l) = return $ Lit l
label (LC.Op o) = case optype o of 
  OpBinLit -> do 
    i <- fst <$> get
    modify (\(i,ls) -> (i+3,ls))
    return $ Lam (varInt $ i+1) $ Lam (varInt $ i+2) $ ULit $ i+3
  OpBinBool -> do
    i <- fst <$> get
    modify (\(i,ls) -> (i+3,ls))
    return $ Lam (varInt $ i+1) $ 
             Lam (varInt $ i+2) $ 
             Lam (varInt $ i+3) $ 
             Lam (varInt $ i+4) $ 
             UBool (varInt $ i+3) (varInt $ i+4)
  OpLitWorld -> do
    i <- fst <$> get
    modify (\(i,ls) -> (i+2,ls))
    return $ Lam (varInt $ i+1) $ Lam (varInt $ i+2) $ Var (varInt $ i+2)
  OpWorldLit -> do
    i <- fst <$> get
    modify (\(i,ls) -> (i+5,ls))
    return $ Lam (varInt $ i+1) 
           $ App (i+2) (ULit (i+3)) 
           $ App (i+4) (ULit (i+5)) (Var (varInt $ i+1))

varInt = (:) 'v' . show

optype o = case o of
  LC.Add -> OpBinLit
  LC.Sub -> OpBinLit
  LC.Mul -> OpBinLit
  LC.Div -> OpBinLit
  LC.Mod -> OpBinLit
  LC.Eq  -> OpBinBool
  LC.Neq  -> OpBinBool
  LC.Lt  -> OpBinBool
  LC.Gt  -> OpBinBool
  LC.Le  -> OpBinBool
  LC.Ge  -> OpBinBool
  LC.Write w -> OpLitWorld
  LC.Read w -> OpWorldLit
  LC.Syscall n -> OpWorldLit
  a -> error $ "Unsupported analysis of: " ++ show a

var :: Expr -> MetaVar 
var (Var v) = V v
var (Lam l e) = L l
var (App l m n) = A l  
var (Lit l) = C l
var (ULit l) = UL l
var (UBool t f) = UB t f

bodies :: Expr -> Bodies
bodies (Lam v e) = M.insert (L v) (var e) $ bodies e 
bodies (App i m n) = M.union (bodies m) (bodies n)
bodies _ = M.empty

ca e = ca' e (bodies e) M.empty where 
  ca' e b mu = if mu == mu' then mu else ca' e b mu' where mu' = m e b mu

m :: Expr -> Bodies -> ClosureAnalysis -> ClosureAnalysis
m (Var v) b mu = mu
m (Lam v e) b mu = m e b mu `union` M.singleton (L v) (S.singleton (LV v))
m (Lit l) b mu = mu `union` M.singleton (C l) (S.singleton (CV l))
m (ULit l) b mu = mu `union` M.singleton (UL l) (S.singleton (ULV l))
m (UBool t f) b mu = mu `union` M.singleton (UB t f) (emptyLu (V t) mu `S.union` emptyLu (V f) mu)
m (App i e1 e2) b mu = 
  m e1 b mu `union` 
  m e2 b mu `union` 
  (M.fromListWith S.union $ concat $ 
       [[(V l, emptyLu (var e2) mu), 
         (A i, emptyLu (fjlu (L l) b) mu)] 
        | LV l <- S.toList $ emptyLu (var e1) mu]
    ++ [[(V l, S.singleton (CV j)),
         (A i, emptyLu (fjlu (L l) b) mu)]
        | LV l <- S.toList $ emptyLu (var e2) mu
        , CV j <- S.toList $ emptyLu (var e1) mu]
    ++ [[(V l, S.singleton (ULV j)), 
         (A i, emptyLu (fjlu (L l) b) mu)]
        | LV l <- S.toList $ emptyLu (var e2) mu
        , ULV j <- S.toList $ emptyLu (var e1) mu])

emptyLu k m = maybe S.empty id $ M.lookup k m 

fv :: Expr -> FreeVariableAnalysis
fv (Var v) = M.singleton (V v) (S.singleton v)
fv (Lam v e) = let fve = fv e in M.insert (L v) (S.filter (/= v) $ fjlu (var e) fve) fve
fv (App l m n) = let fvmn = fv m `M.union` fv n in M.insert (A l) (fjlu (var m) fvmn `S.union` fjlu (var n) fvmn) fvmn
fv (UBool t f) = M.singleton (UB t f) (S.fromList [t,f])
fv a = M.singleton (var a) S.empty

union = M.unionWith S.union
fjlu v = maybe (error $ "unbound var: " ++ show v) id . M.lookup v

ppca :: ClosureAnalysis -> String
ppca m = concat.intersperse ", ".map pp.filter vars.M.toList.M.map S.toList $ m
  where pp (v, ls) = "(" ++ show v ++ ":" ++ (concat . intersperse ", " . map show) ls ++ ")"

vars e = case fst e of V _ -> True; _ -> False

instance Show Value where
  show (LV l) = l
  show (CV i) = show i
  show (ULV l) = '?':show l 

instance Show MetaVar where
  show (V l) = l
  show (A l) = '@':show l
  show (UB t f) = t ++ "?" ++ f
  show (L l) = l
  show (UL l) = '?':show l
  show (C c) = show c

instance Show Expr where
  show (Var v) = v
  show (Lam l e) = "\\" ++ l ++ "." ++ show e
  show (App v m n) = "(" ++ show m ++ " @" ++ show v ++ " " ++ show n ++ ")"
  show (Lit l) = show l
  show (ULit l) = '?':show l
  show (UBool t f) = t ++ "?" ++ f

graphToDot' :: (Ord a) => [(a, [a])] -> GV.DotGraph a
graphToDot' nes = GV.graphElemsToDot (GV.defaultParams :: GV.GraphvizParams a () () () ()) ns es where
  ns = map (\(a,b) -> (a,())) nes
  es = concatMap (\(f,ts) -> map (\t -> (f,t,())) ts) nes

saveCallGraph :: ClosureAnalysis -> Bool -> FilePath -> IO FilePath
saveCallGraph g v = GV.runGraphviz (graphToDot' (map (\(v,vs) -> (show v, map show $ S.toList vs)) . (if v then id else filter vars) $ M.toList g)) GV.Svg 

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances  #-}
module CEM where
import IO 
import LC
import Data.List (sort)
import qualified Data.Map as M

data Stack = Upd Stack Int | Arg Stack Closure | Empty
data Closure = Close DBExpr Int deriving (Eq, Ord)
type Heap = M.Map Int Cell
data Cell = Cell Closure Int | Bullet deriving (Eq, Ord)
data State = State Closure Stack Heap 

fresh :: Heap -> Int
fresh h = maximum (0:M.keys h) + 1

updateClosure :: Int -> Closure -> Heap -> Heap
updateClosure i c h = M.insert i (Cell c e) h where Cell _ e = h M.! i

step :: State -> State
step (State c@(Close t e) s h) = case t of
  Var 0 -> State c (Upd s e) h where Cell c i = h M.! e
  Var n -> State (Close (Var$n-1) e') s h where Cell _ e' = h M.! e
  App m n -> State (Close m e) (Arg s $ Close n e) h
  Lam _ b -> case s of 
    Upd s u -> State c s $ updateClosure u c h
    Arg s a -> State (Close b e') s $ M.insert e' (Cell a e) h where e' = fresh h

instance Pretty Stack where 
  pp Empty = "\\square" 
  pp (Upd s i) = pp s ++ " " ++ show i
  pp (Arg s a) = pp s ++ " " ++ pp a

instance Pretty Closure where
  pp (Close t e) = pp t ++ "[" ++ show e ++ "]"

instance Pretty Heap where
  pp m = pp' $ reverse $ sort $ M.assocs m where
    pp' [] = "\\epsilon"
    pp' ((k,v):m) = pp' m ++ "[" ++ show k ++ "\\mapsto" ++ pp v ++ "]"
    
instance Pretty Cell where
  pp (Cell c i) = pp c ++ "\\cdot" ++ show i
  pp Bullet = "\\bullet"

instance Pretty State where 
  pp (State c s h) = "\\langle " ++ pp c ++ "," ++ pp s ++ "," ++ pp h ++ "\\rangle"

trace :: DBExpr -> IO ()
trace t = do
  putStrLn "\\begin{align}"
  putStrLn $ pp initS ++ "\\\\"
  tr initS 
  putStrLn "\\end{align}"
  where
    initS = State (Close t 0) Empty $ M.singleton 0 Bullet
    tr s@(State (Close (Lam _ _) e) Empty h) = return ()
    tr s = do
      putStr "&\\rightarrow_{\\mathcal{\\mathcal{C} \\mskip -4mu \\mathcal{E}}}" 
      putStr (pp s')
      putStr "\\\\ \n"
      tr s' 
      where s' = step s


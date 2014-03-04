{-#LANGUAGE OverloadedStrings#-}
module VM where

import qualified Data.Set as S
import qualified Data.Map as M
import qualified LC as LC
import IO
import Data.Text.Lazy (pack)
import Control.Monad.State
import Control.Applicative
import Syscall
import Data.Storable
import Foreign.Ptr
import Data.Word
import Data.GraphViz.Types.Monadic
import Data.GraphViz.Attributes.Complete hiding (Lt, label, Label)
import Data.GraphViz.Attributes
import Data.GraphViz.Commands
import Data.Text.Lazy (pack)

type Env = Int
type Closure = (LExpr, Env)
data StackElem = Update Env | Closure Closure
type Stack = [StackElem]
type Cactus = (Int, M.Map Env (String, Closure, Env))
type CEMState = (Closure, Cactus, Stack)
data LExpr = Var Label String
           | Lam Label String LExpr
           | App Label LExpr LExpr
           | Lit Label (Maybe LC.Literal)
           | Op  Label LC.Op
           | Set (S.Set LExpr)

type Label = Int

instance Show LExpr where
  show e = case e of
    Var l s -> s
    Lam l s e -> 'λ':s ++ '.':show e
    App l m n -> "(" ++ show m ++ " " ++ show n ++ ")"
    Lit l i -> case i of Nothing -> "#"; Just i -> show i
    Op l o -> show o

label :: LC.SExpr -> LExpr
label e = evalState (label' e) 0
  where label' :: LC.SExpr  -> State Int LExpr
        label' (LC.Lam l e) = do i<-get; modify succ; e'<-label' e; return $ Lam i l e'
        label' (LC.App m n) = do i<-get; modify succ; m'<-label' m; n'<-label' n; return $ App i m' n'
        label' (LC.Var v) = do i<-get; modify succ; return $ Var i v
        label' (LC.Lit l) = do i<-get; modify succ; return $ Lit i (Just l)
        label' (LC.Op o) = do i<-get; modify succ; return $ Op i o 

vlookup v e h = maybe (error $ "unbound var: " ++ v) match (M.lookup e (snd h))
  where match (v', cl, e') = if v == v' then (cl, e) else vlookup v e' h

update l c h = M.insertWith (\(s,c,e) (s',c',e') -> (s',c,e')) l ("",c,-1) h

cem :: CEMState -> IO CEMState
cem ((Var i v, e), h, s) = let (cl, e') = vlookup v e h in
  return (cl, h, Update e':s)
cem ((App i m n, e), h, s) = 
  return ((m,e),h, Closure (n,e):s)
cem ((Lam i v e', e), (n,h), Closure c:cs) = 
  return ((e', (n+1)), (n+1, M.insert (n+1) (v,c,e) h), cs)
cem ((Lam i v e', e), (n,h), Update loc:cs) = 
  return ((Lam i v e', e), (n,update loc (Lam i v e', e) h), cs)
cem ((Lit i l, e), (n,h), Closure c:cs) = 
  return (c, (n,h), Closure ((Lit i l),e):cs)
cem ((Lit i l, e), (n,h), Update loc:cs) = 
  return ((Lit i l, e), (n,update loc (Lit i l, e) h), cs)
cem ((Op i o, e), h, cs) = (\(t, cs') -> ((t, e), h, cs')) <$> 
  case o of     
    LC.Syscall n -> (,drop (n+1) cs) . Lit i . Just <$> syscall n t [i | Closure (Lit _ (Just i), e) <- take n $ tail cs]
    LC.Write w -> case w of
      LC.Word8 -> (,drop 2 cs) <$> (pokeV (intPtrToPtr $ toEnum t) (toEnum t' :: Word8) >> (return $ label $ defParse "\\x.x"))
      LC.Word64 -> (,drop 2 cs) <$> (pokeV (intPtrToPtr $ toEnum t) (toEnum t' :: Word64) >> (return $ label $ defParse "\\x.x"))
    LC.Read w -> case w of
      LC.Word8 -> (,tail cs) <$> (peekV (intPtrToPtr $ toEnum t) >>= return . Lit i . Just)
      LC.Word64 -> (,tail cs) <$> (peekV (intPtrToPtr $ toEnum t) >>= return . Lit i . Just)
    a -> return $ (,cs') $ case a of 
      LC.Add -> Lit i $ (+) <$> arg1 <*> arg2
      LC.Sub -> Lit i $ (-) <$> arg1 <*> arg2
      LC.Mul -> Lit i $ (*) <$> arg1 <*> arg2
      LC.Div -> Lit i $ div <$> arg1 <*> arg2
      LC.Mod -> Lit i $ mod <$> arg1 <*> arg2
      LC.Le -> toChurch $ (<=) t' t
      LC.Ge -> toChurch $ (>=) t' t
      LC.Lt -> toChurch $ (<) t' t
      LC.Gt -> toChurch $ (>) t' t
      LC.Eq -> toChurch $ (==) t' t
      LC.Neq -> toChurch $ (/=) t' t
      where (Closure (Lit _ arg2, _)):(Closure (Lit _ arg1, _)):cs' = cs
    where (Closure (Lit _ (Just t), _)):(Closure (Lit _ (Just t'), _)):cs' = cs

toChurch b = if b then label true else label false

traceCEM :: (CEMState -> IO ()) -> CEMState -> IO CEMState
traceCEM pp m = case m of 
  s@((Lam i v e', e), h, []) -> pp m >> return s
  s@((Lit i l, e), h, []) -> pp m >> return s 
  _ -> pp m >> cem m >>= traceCEM pp

instance Show StackElem where
    show (Closure (c,e)) = show c ++ "[" ++ show e ++ "]"
    show (Update e) = show e ++ ":="

showGraph fname dg = do
  runGraphviz dg DotOutput ("/tmp/" ++ fname)
  runGraphvizCanvas' dg Xlib

todot ((c,e), (i,h), s) = digraph (Str "State") $ do
  --graphAttrs [FontName "DroidSerif"]
  nodeAttrs [] --[FontName "DroidSerif"]
  cluster (Str "Closure") $ do
    graphAttrs [textLabel "Closure"]
    node (-2) [textLabel (pack.show$Closure (c,e)), 
               color White]
  cluster (Str "Context") $ do
    graphAttrs [textLabel "Context"]
    node (-1) [ style filled, 
                color White, 
                shape Record, 
                toLabel $ FlipFields (map (FieldLabel . pack . show) s ++ [FieldLabel "□"])]
  cluster (Str "Heap") $ do
    graphAttrs [textLabel "Heap"]
    node 0 [shape Record, textLabel "0=•"]
    let nodes = M.toList h
    mapM (\(i,(s,(c,e'),e)) -> node i [shape Record, textLabel (pack $ show i ++ "↦" ++ s ++ "=" ++ show c)]) nodes
    mapM (\(i,(s,(c,e'),e)) -> edge i e []) nodes 
    mapM (\(i,(s,(c,e'),e)) -> edge i e' [style dashed]) nodes 

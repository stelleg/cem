{-#LANGUAGE OverloadedStrings#-}
{-#LANGUAGE TupleSections#-}
module VM where

import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.IntMap as IM
import qualified LC as LC
import IO
import Data.Text.Lazy (pack)
import Control.Monad.State
import Control.Applicative
import Syscall
import Foreign.Storable
import Foreign.Ptr
import Data.Word
import Data.GraphViz.Types.Monadic
import Data.GraphViz.Attributes.Complete hiding (Lt, label, Label)
import Data.GraphViz.Attributes
import Data.GraphViz.Commands
import Data.GraphViz (DotGraph)
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
           | World Label

instance Eq LExpr where
  e1 == e2 = getLabel e1 == getLabel e2
--  Lit l e == Lit l' e' = l == l' && e == e'

instance Ord LExpr where
  e1 `compare` e2 = getLabel e1 `compare` getLabel e2
--  Lit l e `compare` Lit l' e' = case l `compare` l' 
--    of EQ -> e `compare` e'
--       ord -> ord

type Label = Int

expr :: LExpr -> IM.IntMap LExpr
expr e = case e of
  Var l v -> IM.singleton l e
  App l m n -> IM.insert l e (expr m `IM.union` expr n)
  Lam l v e' -> IM.insert l e (expr e')
  Lit l i -> IM.insert l e $ expr $ litval l
  Op l o -> case o of
    LC.Syscall n -> IM.insert l e $ expr $ returnval l
    LC.Write w -> IM.insert l e $ expr $ returnval l 
    LC.Read w -> IM.insert l e $ expr $ returnval l
    LC.Add -> IM.insert l e $ expr $ litval l 
    LC.Sub -> IM.insert l e $ expr $ litval l 
    LC.Mul -> IM.insert l e $ expr $ litval l
    LC.Div -> IM.insert l e $ expr $ litval l 
    LC.Mod -> IM.insert l e $ expr $ litval l 
    LC.Le -> IM.singleton l e 
    LC.Ge -> IM.singleton l e
    LC.Lt -> IM.singleton l e
    LC.Gt -> IM.singleton l e
    LC.Eq -> IM.singleton l e
    LC.Neq -> IM.singleton l e
  World l -> IM.insert l e $ expr $ worldval l

returnval :: Int -> LExpr
returnval l = Lam (l+1) "f" $ App (l+2) (App (l+3) (Var (l+4) "f") (Lit (l+5) Nothing)) (World $ l+6) 

litval :: Int -> LExpr
litval l = Lam (l+1) "f" $ App (l+2) (Var (l+3) "f") (Lit (l+4) Nothing)

worldval :: Int -> LExpr
worldval l = Lam (l+1) "f" $ App (l+2) (Var (l+3) "f") (World $ l+4)

showIndex :: Int -> String
showIndex i = map (toEnum . (+ 8272) . fromEnum) $ show i 

instance Show LExpr where
  show e = show' Z e where
    show' p e = case e of
      Var l s -> s 
      Lam l s e -> parensIf (p==L) $ "λ" ++ s ++ '.':show' Z e
      App l m n -> parensIf (p==R) $ show' L m ++ " " ++ show' R n
      Lit l i -> case i of Nothing -> "#"; Just i -> show i
      Op l o -> show o 
      World l -> "Ω"

showlabeled e = case e of
  Var l s -> s ++ showIndex l
  Lam l s e -> 'λ':showIndex l ++ s ++ '.':showlabeled e
  App l m n -> '(':showIndex l ++ showlabeled m ++ " " ++ showlabeled n ++ ")"
  Lit l i -> (case i of Nothing -> "#"; Just i -> show i) ++ showIndex l
  Op l o -> show o ++ showIndex l
  World l -> "Ω" ++ showIndex l 

getLabel :: LExpr -> Label
getLabel e = case e of
  Var l v -> l
  Lam l v e -> l
  App l m n -> l
  Lit l i -> l
  Op l o -> l
  World l -> l

labeled :: LC.SExpr -> LExpr
labeled e = evalState (labeled' e) 0
  where labeled' :: LC.SExpr  -> State Int LExpr
        labeled' (LC.Lam l e) = Lam <$> incr 1 <*> pure l <*> labeled' e
        labeled' (LC.App m n) = App <$> incr 2 <*> labeled' m <*> labeled' n
        labeled' (LC.Var v) = Var <$> incr 1 <*> pure v
        labeled' (LC.Lit l) = Lit <$> incr 5 <*> pure (Just l)
        labeled' (LC.Op o) = Op <$> incr 7 <*> pure o  -- worst case
        labeled' LC.World = World <$> incr 5 
        incr n = do i <- get; put (i+n); return i

relabeled :: LExpr -> LExpr
relabeled e = evalState (labeled' e) 0
  where labeled' :: LExpr  -> State Int LExpr
        labeled' (Lam _ v e) = Lam <$> incr 1 <*> pure v <*> labeled' e
        labeled' (App _ m n) = App <$> incr 2 <*> labeled' m <*> labeled' n
        labeled' (Var _ v) = Var <$> incr 1 <*> pure v
        labeled' (Lit _ i) = Lit <$> incr 1 <*> pure i
        labeled' (Op _ o) = Op <$> incr 7 <*> pure o  -- worst case
        labeled' (World _) = World <$> incr 1
        incr n = do i <- get; put (i+n); return i

vlookup v e h = maybe (error $ "unbound var: " ++ v) match (M.lookup e (snd h))
  where match (v', cl, e') = if v == v' then (cl, e) else vlookup v e' h

env e h = maybe [] next $ M.lookup e (snd h)
  where next (v', cl, e') = cl:env e' h

update l c h = M.insertWith (\(s,c,e) (s',c',e') -> (s',c,e')) l ("",c,-1) h

isValue (App _ _ _) = False
isValue (Var _ _) = False
isValue (Op _ _)  = False
isValue _ = True

cem :: CEMState -> IO CEMState
cem ((c,e), (n,h), Update loc:s) | isValue c = 
  return ((c,e), (n,update loc (c,e) h), s)
cem ((Var i v, e), h, s) = let (cl, e') = vlookup v e h in
  return (cl, h, Update e':s)
cem ((App i m n, e), h, s) = 
  return ((m,e),h, Closure (n,e):s)
cem ((Lam i v e', e), (n,h), Closure c:cs) = 
  return ((e', (n+1)), (n+1, M.insert (n+1) (v,c,e) h), cs)
cem ((Lit i l, e), (n,h), Closure c:cs) = 
  return (c, (n,h), Closure (Lit i l,e):cs)
cem ((World i, e), (n,h), Closure c:cs) = 
  return (c, (n,h), Closure (World i,e):cs)
cem ((Op i o, e), h, cs) = (\(t, e', cs') -> ((t, e'), h, cs')) <$> 
  case o of     
    LC.Syscall n -> (,e,cs) . pair' i (World i) . Lit i . Just <$> syscall n [i | (Lit _ (Just i), e) <- take (n+1) $ env']
      where (World _,_):env' = env e h
    LC.Write w -> case w of
      LC.Word8 ->  poke (intPtrToPtr $ toEnum t) (toEnum t' :: Word8) >> return (pair' i (World i) (labeled lid), e, cs)
      LC.Word16 ->  poke (intPtrToPtr $ toEnum t) (toEnum t' :: Word16) >> return (pair' i (World i) (labeled lid), e, cs)
      LC.Word32 ->  poke (intPtrToPtr $ toEnum t) (toEnum t' :: Word32) >> return (pair' i (World i) (labeled lid), e, cs)
      LC.Word64 -> poke (intPtrToPtr $ toEnum t) (toEnum t' :: Word64) >> return (pair' i (World i) (labeled lid), e, cs)
      where (World _,_):(Lit _ (Just t), _):(Lit _ (Just t'), _):e' = env e h
    LC.Read w -> case w of
      LC.Word8 -> (,e,cs) <$> (peek (intPtrToPtr $ toEnum t) >>= return . pair' i (World i) . Lit i . Just . (fromEnum :: Word8 -> Int))
      LC.Word16 -> (,e,cs) <$> (peek (intPtrToPtr $ toEnum t) >>= return . pair' i (World i) . Lit i . Just . (fromEnum :: Word16 -> Int))
      LC.Word32 -> (,e,cs) <$> (peek (intPtrToPtr $ toEnum t) >>= return . pair' i (World i) . Lit i . Just . (fromEnum :: Word32 -> Int))
      LC.Word64 -> (,e,cs) <$> (peek (intPtrToPtr $ toEnum t) >>= return . pair' i (World i) . Lit i . Just . (fromEnum :: Word64 -> Int))
      where (World _,_):(Lit _ (Just t), _):e' = env e h
    a -> return $ case a of 
      LC.Add -> (,e,cs) <$> Lit i $ (+) <$> arg1 <*> arg2 where (Lit _ arg2, _):(Lit _ arg1, _):e' = env e h
      LC.Sub -> (,e,cs) <$> Lit i $ (-) <$> arg1 <*> arg2 where (Lit _ arg2, _):(Lit _ arg1, _):e' = env e h
      LC.Mul -> (,e,cs) <$> Lit i $ (*) <$> arg1 <*> arg2 where (Lit _ arg2, _):(Lit _ arg1, _):e' = env e h
      LC.Div -> (,e,cs) <$> Lit i $ div <$> arg1 <*> arg2 where (Lit _ arg2, _):(Lit _ arg1, _):e' = env e h
      LC.Mod -> (,e,cs) <$> Lit i $ mod <$> arg1 <*> arg2 where (Lit _ arg2, _):(Lit _ arg1, _):e' = env e h
      LC.Le -> toChurch ((<=) t' t) ct cf cs where (Lit _ (Just t), _):(Lit _ (Just t'), _):cf:ct:e' = env e h
      LC.Ge -> toChurch ((>=) t' t) ct cf cs where (Lit _ (Just t), _):(Lit _ (Just t'), _):cf:ct:e' = env e h
      LC.Lt -> toChurch ((<) t' t) ct cf cs where (Lit _ (Just t), _):(Lit _ (Just t'), _):cf:ct:e' = env e h
      LC.Gt -> toChurch ((>) t' t) ct cf cs where (Lit _ (Just t), _):(Lit _ (Just t'), _):cf:ct:e' = env e h
      LC.Eq -> toChurch ((==) t' t) ct cf cs where (Lit _ (Just t), _):(Lit _ (Just t'), _):cf:ct:e' = env e h
      LC.Neq -> toChurch ((/=) t' t) ct cf cs  where (Lit _ (Just t), _):(Lit _ (Just t'), _):cf:ct:e' = env e h

pair' l f s = App l (App l (labeled pair) s) f
toChurch b (ct,et) (cf,ef) cs = if b then (ct,et,cs) else (cf,ef,cs)

traceCEM :: (CEMState -> IO ()) -> CEMState -> IO CEMState
traceCEM pp m = case m of 
  s@((World i, e), h, []) -> pp m >> return s
  s@((Lam i v e', e), h, []) -> pp m >> return s
  s@((Lit i l, e), h, []) -> pp m >> return s 
  _ -> pp m >> cem m >>= traceCEM pp

instance Show StackElem where
    show (Closure (c,e)) = show c ++ "[" ++ show e ++ "]"
    show (Update e) = show e

showGraph fname dg = do
  runGraphviz dg DotOutput ("/tmp/" ++ fname)
  runGraphvizCanvas' dg Xlib

todot ((c,e), (i,h), s) = digraph (Str "State") $ do
  nodeAttrs []
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
    mapM (\(i,(s,(c,e'),e)) -> node i [shape Record, textLabel (pack . process $ show i ++ " : " ++ s ++ " = " ++ show c)]) nodes
    mapM (\(i,(s,(c,e'),e)) -> edge i e []) nodes 
    mapM (\(i,(s,(c,e'),e)) -> edge i e' [style dashed]) nodes 

process s = filter f s
  where f '>' = False
        f '<' = False
        f  c =  True
      

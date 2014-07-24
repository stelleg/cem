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
           | World Label

type Label = Int

expr :: LExpr -> IM.IntMap LExpr
expr e = case e of
  Var l v -> IM.singleton l $ Var l v
  App l m n -> IM.insert l (App l m n) (expr m `IM.union` expr n)
  Lam l v e -> IM.insert l (Lam l v e) (expr e)
  Lit l i -> IM.singleton l $ Lit l i
  Op l o -> IM.singleton l $ Op l o

showIndex :: Int -> String
showIndex i = map (toEnum . (+ 8272) . fromEnum) $ show i 

instance Show LExpr where
  show e = case e of
    Var l s -> s ++ showIndex l
    Lam l s e -> 'λ':showIndex l ++ s ++ '.':show e
    App l m n -> '(':showIndex l ++ show m ++ " " ++ show n ++ ")"
    Lit l i -> (case i of Nothing -> "#"; Just i -> show i) ++ showIndex l
    Op l o -> show o ++ showIndex l
    World l -> "Ω" ++ showIndex l 

showlabeled e = case e of
    Var l s -> s ++ "_" ++ show l
    Lam l s e -> "λ_" ++ show l ++ s ++ '.':showlabeled e
    App l m n -> "(_" ++ show l ++ showlabeled m ++ " " ++ showlabeled n ++ ")"
    Lit l i -> (case i of Nothing -> "#"; Just i -> show i) ++ "_" ++ show l
    Op l o -> show o ++ "_" ++ show l
    World w -> "Ω_" ++ show w

getLabel :: LExpr -> Label
getLabel e = case e of
  Var l v -> l
  Lam l v e -> l
  App l m n -> l
  Lit l i -> l
  Op l o -> l

labeled :: LC.SExpr -> LExpr
labeled e = evalState (labeled' e) 0
  where labeled' :: LC.SExpr  -> State Int LExpr
        labeled' (LC.Lam l e) = Lam <$> incr <*> pure l <*> labeled' e
        labeled' (LC.App m n) = App <$> incr <*> labeled' m <*> labeled' n
        labeled' (LC.Var v) = Var <$> incr <*> pure v
        labeled' (LC.Lit l) = Lit <$> incr <*> pure (Just l)
        labeled' (LC.Op o) = Op <$> incr <*> pure o 
        labeled' LC.World = World <$> incr
        incr = do i <- get; put (i+1); return i

vlookup v e h = maybe (error $ "unbound var: " ++ v) match (M.lookup e (snd h))
  where match (v', cl, e') = if v == v' then (cl, e) else vlookup v e' h

env e h = maybe [] next $ M.lookup e (snd h)
  where next (v', cl, e') = cl:env e' h

update l c h = M.insertWith (\(s,c,e) (s',c',e') -> (s',c,e')) l ("",c,-1) h

isValue (App _ _ _) = False
isValue (Var _ _) = False
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
      LC.Word8 ->  pokeV (intPtrToPtr $ toEnum t) (toEnum t' :: Word8) >> return (pair' i (World i) (labeled lid), e, cs)
      LC.Word16 ->  pokeV (intPtrToPtr $ toEnum t) (toEnum t' :: Word16) >> return (pair' i (World i) (labeled lid), e, cs)
      LC.Word32 ->  pokeV (intPtrToPtr $ toEnum t) (toEnum t' :: Word32) >> return (pair' i (World i) (labeled lid), e, cs)
      LC.Word64 -> pokeV (intPtrToPtr $ toEnum t) (toEnum t' :: Word64) >> return (pair' i (World i) (labeled lid), e, cs)
      where (World _,_):(Lit _ (Just t), _):(Lit _ (Just t'), _):e' = env e h
    LC.Read w -> case w of
      LC.Word8 -> (,e,cs) <$> (peekV (intPtrToPtr $ toEnum t) >>= return . pair' i (World i) . Lit i . Just . (fromEnum :: Word8 -> Int))
      LC.Word16 -> (,e,cs) <$> (peekV (intPtrToPtr $ toEnum t) >>= return . pair' i (World i) . Lit i . Just . (fromEnum :: Word16 -> Int))
      LC.Word32 -> (,e,cs) <$> (peekV (intPtrToPtr $ toEnum t) >>= return . pair' i (World i) . Lit i . Just . (fromEnum :: Word32 -> Int))
      LC.Word64 -> (,e,cs) <$> (peekV (intPtrToPtr $ toEnum t) >>= return . pair' i (World i) . Lit i . Just . (fromEnum :: Word64 -> Int))
      where (World _,_):(Lit _ (Just t), _):e' = env e h
    a -> return $ case a of 
      LC.Add -> (,e,cs) <$> Lit i $ (+) <$> arg1 <*> arg2
      LC.Sub -> (,e,cs) <$> Lit i $ (-) <$> arg1 <*> arg2
      LC.Mul -> (,e,cs) <$> Lit i $ (*) <$> arg1 <*> arg2
      LC.Div -> (,e,cs) <$> Lit i $ div <$> arg1 <*> arg2
      LC.Mod -> (,e,cs) <$> Lit i $ mod <$> arg1 <*> arg2
      LC.Le -> toChurch ((<=) t' t) ct cf cs
      LC.Ge -> toChurch ((>=) t' t) ct cf cs
      LC.Lt -> toChurch ((<) t' t) ct cf cs
      LC.Gt -> toChurch ((>) t' t) ct cf cs
      LC.Eq -> toChurch ((==) t' t) ct cf cs
      LC.Neq -> toChurch ((/=) t' t) ct cf cs
      where (Lit _ arg2, _):(Lit _ arg1, _):e' = env e h
      where (Lit _ (Just t), _):(Lit _ (Just t'), _):ct:cf:e' = env e h

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
    show (Update e) = show e ++ ":="

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
    mapM (\(i,(s,(c,e'),e)) -> node i [shape Record, textLabel (pack $ show i ++ "↦" ++ s ++ "=" ++ show c)]) nodes
    mapM (\(i,(s,(c,e'),e)) -> edge i e []) nodes 
    mapM (\(i,(s,(c,e'),e)) -> edge i e' [style dashed]) nodes 

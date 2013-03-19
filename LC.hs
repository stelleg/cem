{-#LANGUAGE GADTs #-}
module LC where
import Text.ParserCombinators.Parsec
import Data.Either
import Control.Monad
import Data.Maybe

data Expr a where
  Var :: a -> Expr a
  App :: Expr a -> Expr a -> Expr a
  Lam :: a -> Expr a -> Expr a

instance Show a => Show (Expr a) where
  show (Var s)     = show s
  show (Lam s e)   = "\\" ++ show s  ++ "." ++ show e
  show (App e1 e2) = "("  ++ show e1 ++ " " ++ show e2 ++ ")" 

lc :: Parser (Expr String)
lc = do lam
    <|> app 
    <|> var

lam = do char '\\'; x <- word; char '.'; spaces;   e <- lc;  return $ Lam x e
app = do char '(';  e1 <- lc;  spaces;   e2 <- lc; char ')'; return $ App e1 e2
var = do word >>= return.Var
word = many letter

parseFile :: String -> IO (Expr Int)
parseFile filename = do 
  source <- readFile filename
  let e = case parse lc "" source of
            Left err -> error $ show err
            Right e -> e
  return $ deBruijn e []

deBruijn :: Expr String -> [(String, Int)] -> Expr Int
deBruijn (Var v)     ls = Var $ fromJust $ lookup v ls
deBruijn (Lam x t)   ls = Lam 0 $ deBruijn t $ (x, 0):[(x,y+1) | (x,y) <- ls]
deBruijn (App t1 t2) ls = App (deBruijn t1 ls) (deBruijn t2 ls)

compile :: Expr Int -> [Instr]
compile (Lam i e') = TAKE 0 : compile e'
compile (Var i)    = [ENTER i]
compile (App m n)  = PUSH (length ms + 1) : ms ++ compile n
  where ms = compile m

deCompile :: [Instr] -> Expr Int
deCompile (PUSH  i:is) = App (deCompile is) (deCompile (drop (i-1) is))
deCompile (TAKE  i:is) = Lam i $ deCompile is
deCompile (ENTER i:is) = Var i

data Instr = 
    PUSH Int
  | TAKE Int
  | ENTER Int 
  deriving (Show, Eq)

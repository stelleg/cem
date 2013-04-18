{-#LANGUAGE GADTs #-}
module LC where
import Text.ParserCombinators.Parsec
import Data.Either
import Control.Monad
import Data.Maybe
import Control.Applicative ((<$>))

data Expr a where
  Var :: a -> Expr a
  App :: Expr a -> Expr a -> Expr a
  Lam :: a -> Expr a -> Expr a
  Lit :: Literal -> Expr a
  Op  :: Op -> Expr a

type Literal = Int

data Op = Add | Sub | Mul | Div | Eq | Lt | Gt | Le | Ge deriving (Enum, Eq)

instance Show Op where
  show o = case o of
    Add -> "+"
    Sub -> "-"
    Div -> "/"
    Mul -> "*"
    Eq  -> "="
    Lt  -> "<"
    Gt  -> ">"
    Le  -> "<="
    Ge  -> ">="

instance Show a => Show (Expr a) where
  show (Var s)     = show s
  show (Lam s e)   = "\\" ++ show s  ++ "." ++ show e
  show (App e1 e2) = "("  ++ show e1 ++ " " ++ show e2 ++ ")" 
  show (Lit l) = show l
  show (Op o) = show o

lc :: Parser (Expr String)
lc =  lam
  <|> app 
  <|> var
  <|> lit
  <|> op 

lam = do char '\\'; x <- word; char '.'; spaces;   e <- lc;  return $ Lam x e
app = do char '(';  e1 <- lc;  spaces;   e2 <- lc; char ')'; return $ App e1 e2
var = Var <$> word
lit = Lit . read <$> many1 digit
op  = Op . opString <$> many1 (oneOf "+-*/=<>")

word = many1 letter

opString c = case c of
  "+" -> Add
  "-" -> Sub
  "*" -> Mul
  "/" -> Div
  "=" -> Eq
  "<" -> Lt
  ">" -> Gt
  "<=" -> Le
  ">=" -> Ge

parseFile :: String -> IO (Expr Int)
parseFile filename = do 
  source <- readFile filename
  let e = case parse lc "" source of
            Left err -> error $ show err
            Right e -> e
  return $ deBruijn e []

deBruijn :: Expr String -> [(String, Int)] -> Expr Int
deBruijn (Var v)     ls = case lookup v ls of 
  Just i -> Var i; 
  Nothing -> error $ "unbound var " ++ v;
deBruijn (Lam x t)   ls = Lam 0 $ deBruijn t $ (x, 0):[(x,y+1) | (x,y) <- ls]
deBruijn (App t1 t2) ls = App (deBruijn t1 ls) (deBruijn t2 ls)
deBruijn (Lit l) ls = Lit l
deBruijn (Op o) ls = Op o

compile :: Expr Int -> [Instr]
compile (Lam i e') = TAKE 0 : compile e'
compile (Var i)    = [ENTER i]
compile (App m n)  = PUSH (length ms + 1) : ms ++ compile n
  where ms = compile m
compile (Lit l)    = [LIT l]
compile (Op o)     = [OP o]

deCompile :: [Instr] -> Expr Int
deCompile (PUSH  i:is) = App (deCompile is) (deCompile (drop (i-1) is))
deCompile (TAKE  i:is) = Lam i $ deCompile is
deCompile (ENTER i:is) = Var i

data Instr = 
    PUSH Int
  | TAKE Int
  | ENTER Int 
  | LIT Literal
  | OP Op
  deriving (Show, Eq)

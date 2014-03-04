{-# LANGUAGE FlexibleInstances #-}
module IO where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Token
import Text.ParserCombinators.Parsec.Prim (getPosition)
import Data.Either
import Data.Maybe
import Data.Char (isSpace)
import qualified Data.ByteString.Lazy as B
import Control.Monad
import Control.Applicative hiding ((<|>), many)
import Text.Printf
import LC
import qualified Data.Set as S


-- OUTPUT
instance Show Op where
  show o = case o of
    Add -> "+"
    Sub -> "-"
    Div -> "/"
    Mul -> "*"
    Mod -> "%"
    Eq  -> "="
    Neq  -> "/="
    Lt  -> "<"
    Gt  -> ">"
    Le  -> "<="
    Ge  -> ">="
    Write w -> "@" ++ show w
    Read w -> "$" ++ show w
    Call s i -> "; " ++ s ++ show i
    Syscall i -> "!"

instance Show WordSize where
  show ws = case ws of
    Word8 -> "b"
    Word16 -> "s"
    Word32 -> "l"
    Word64 -> "q"

instance Show (SExpr) where
  show (Var s)     = s
  show (Lam s e)   = "λ" ++ s  ++ "." ++ show e
  show (App e1 e2) = "("  ++ show e1 ++ " " ++ show e2 ++ ")" 
  show (Lit l) = show l
  show (Op o) = show o

instance Show (DBExpr) where
  show (Var s)     = show s
  show (Lam s e)   = "λ" ++ show e
  show (App e1 e2) = "("  ++ show e1 ++ " " ++ show e2 ++ ")" 
  show (Lit l) = show l
  show (Op o) = show o

-- INPUT
word = (:) <$> letter <*> many (satisfy $ \c-> not (isSpace c || elem c ".()[]={}"))
literal = (read <$> many1 digit) :: Parser Int

comment = char '#' >> skipMany (noneOf "\n") >> char '\n'
notCode = many ((comment <|> space) >> notCode) >> return ()

pa <^> pb = pa <* notCode <*> pb 
pa ^> pb  = (pa >> notCode) *> pb
pa <^ pb  = pa <* (notCode >> pb)
infixl 4 <^>, <^, ^>

parseSource :: Parser SExpr -> String -> SExpr
parseSource p src = either (error.show) id . parse p "" $ src

lc :: Parser (SExpr)
lc =  Lam <$> ((char '\\' <|> char 'λ') *> word <* char '.') <^> lc
  <|> Var <$> word
  <|> Lit <$> literal
  <|> Op  <$> op
  <|> char '(' ^> (foldl1 App <$> many1 (notCode *> lc <* notCode)) <^ char ')'
  <|> char '[' ^> (foldr1 App <$> many1 (notCode *> lc <* notCode)) <^ char ']'
  <|> char '\'' *> charLit <* char '\''
  <|> char '\"' *> (foldr (\h t -> App (App cons h) t) nil <$> many charLit) <* char '\"'

sugar = (mylet <$> word <^ char '=' <^> lc <^> sugar) <|> lc
  where mylet var val body = App (Lam var body) val

op = do
  opstr <- (:) <$> oneOf "+-*/=<>%@;$!" <*> many (oneOf "+-*/=<>%@;$!qlbs")
  case opstr of
    ":" -> Call <$> word <*> literal
    "!" -> Syscall <$> literal
    s   -> return $ opString s
   
opString s = case s of
  "+" -> Add
  "-" -> Sub
  "*" -> Mul
  "/" -> Div
  "%" -> Mod
  "=" -> Eq
  "/=" -> Neq
  "<" -> Lt
  ">" -> Gt
  "<=" -> Le
  ">=" -> Ge
  ['@',w] -> Write $ wordSizeChar w
  ['$',w] -> Read $ wordSizeChar w
  other -> error other

wordSizeChar c = case c of 
  'b' -> Word8
  's' -> Word16
  'l' -> Word32
  'q' -> Word64

-- Useful expressions
defParse = parseSource sugar

cons = defParse "\\h.\\t.\\n.\\c.(c h t)"
nil = defParse "\\n.\\c.n"

true = defParse "\\t.\\f.t"
false = defParse "\\t.\\f.f"

y = defParse "\\g.(\\x.[g x x] \\x.[g x x])"

charLit :: Parser (SExpr)
charLit = Lit . fromEnum <$> (noneOf "\\\'\"" <|>
                             (special <$> (char '\\' >> oneOf "abfnrtv\\\"\'\&")))
  where special c = case c of
                      't'  -> '\t'
                      'r'  -> '\r'
                      'n'  -> '\n'
                      '\'' -> '\''
                      '\"' -> '\"'
                      _ -> error $ "Couldn't handle special char: " ++ [c]

-- Assembly                      
data Instr = 
    PUSH Int
  | TAKE
  | POP
  | ENTER Int 
  | TAIL Int
  | LIT Literal
  | OP Op
  deriving (Show, Eq)

toMacros :: [Instr] -> [String] -> [String]
toMacros is ds = zipWith3 toMacro [0..] is ds

toMacro :: Int -> Instr -> String -> String
toMacro n i s = case i of 
  PUSH m -> printf "APP L%d entered_L%d" n (m+n)
  ENTER i -> printf "VAR L%d %d # %s" n i s
  TAKE -> printf "LAM L%d # %s" n s
  POP -> printf "UNUSED_LAM L%d # %s" n s
  LIT i -> printf "CONST L%d %d" n i
  OP (Syscall i) -> concat $ [printf "entered_L%d:\n" n] 
                          ++ [printf "LAM sys_lam_%d_%d\n" n k | k <- [0..i]] 
                          ++ [printf "LOADVAR %s\n" (syscallregs!!k) | k <- [0..i-1]]
                          ++ [printf "OP_SYSCALL L%d" n]
  OP o -> flip (printf "OP_%s L%d") n $ case o of
    Add -> "ADD"
    Sub -> "SUB"
    Div -> "DIV"
    Mul -> "MUL"
    Mod -> "MOD"
    Eq  -> "EQ"
    Neq  -> "NEQ"
    Lt  -> "LT"
    Gt  -> "GT"
    Le  -> "LTE"
    Ge  -> "GTE"
    Write w -> "WRITE" ++ show w
    Read w -> "READ" ++ show w

-- This is x64 specific, need to make cross platform
syscallregs = ["%rdi", "%rsi", "%rdx", "%r10", "%r8", "%r9"]

compile :: DBExpr -> [Instr]
compile (Lam _ e) | bound 0 e == 0 = POP : compile (dec 0 e)
compile (Lam _ e)  = TAKE : compile e
compile (Var i)    = [ENTER i]
compile (App m n)  = PUSH (length ms + 1) : ms ++ compile n where ms = compile m
compile (Lit l)    = [LIT l]
compile (Op o)     = [OP o]

bound i (Lam _ e) = bound (i+1) e
bound i (Var i') = if i == i' then 1 else 0
bound i (App m n) = bound i m + bound i n
bound i _ = 0

dec i (Lam _ e) = Lam () (dec (i+1) e)
dec i (Var i') | i' == i = error "POP opt failed"
               | i' > i = Var (i'-1)
               | i' < i = Var i'
dec i (App m n) = App (dec i m) (dec i n)
dec i t = t

debugging :: SExpr -> [String]
debugging (Lam v e)  = v:debugging e
debugging (Var v)    = [v]
debugging (App m n)  = "":debugging m ++ debugging n
debugging (Lit l)    = [""]
debugging (Op o)     = [""]


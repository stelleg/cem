{-# LANGUAGE FlexibleInstances #-}
module IO where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Token (decimal)
import Text.ParserCombinators.Parsec.Prim (getPosition)
import Data.Either
import Data.Maybe
import Data.Char (isSpace, isDigit)
import qualified Data.ByteString.Lazy as B
import Control.Monad
import Control.Applicative hiding ((<|>), many)
import Text.Printf
import LC
import DBUtils
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
  show World = "Ω"

instance Show (DBExpr) where
  show (Var s)     = show s
  show (Lam s e)   = "λ" ++ show e
  show (App e1 e2) = "("  ++ show e1 ++ " " ++ show e2 ++ ")" 
  show (Lit l) = show l
  show (Op o) = show o
  show World = "Ω"

-- INPUT
word :: Parser String
word = (:) <$> satisfy (\c -> not (isSpace c || isDigit c || elem c "\'\"\\.#()[]{}_Ω"))
       <*> many (satisfy $ \c-> not (isSpace c || elem c "\\.()[]{}#"))

literal :: Parser Int
literal = try hex <|> decim

hex :: Parser Int
hex = read <$> ((++) <$> ((:) <$> char '0' <*> (string "x" <|> string "X")) <*> many1 (satisfy hexchar))
  where hexchar c = isDigit c || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')

decim :: Parser Int
decim = read <$> many1 digit

comment = char '#' >> skipMany (noneOf "\n") >> char '\n'
notCode = ((comment <|> space) >> notCode) <|> return ()

pa <^> pb = pa <* notCode <*> pb 
pa ^> pb  = (pa >> notCode) *> pb
pa <^ pb  = pa <* (notCode >> pb)
infixl 4 <^>, <^, ^>

parseProgram :: String -> SExpr 
parseProgram s = parseSource lc $ "{" ++ s ++ "}" ++ "(main Ω \\v.\\w.w)"

parseSource :: Parser SExpr -> String -> SExpr
parseSource p src = either (error.show) id . parse p "" $ src

lc :: Parser SExpr
lc =  Lam <$> ((char '\\' <|> char 'λ') *> word <* char '.') <^> lc
  <|> Lit <$> literal
  <|> Op  <$> op
  <|> Var <$> word
  <|> World <$ char 'Ω'
  <|> char '(' ^> (foldl1 App <$> many1 (notCode *> lc <* notCode)) <^ char ')'
  <|> char '[' ^> (foldr1 App <$> many1 (notCode *> lc <* notCode)) <^ char ']'
  <|> char '\'' *> charLit <* char '\''
  <|> char '\"' *> (foldr (\h t -> App (App cons h) t) nil <$> many charLit) <* char '\"'
  <|> char '{' ^> (lets <$> many (notCode *> binding <* notCode) <^> (char '}' ^> lc))
      where lets = flip $ foldr ($) 
            binding = mylet <$> word <^> (char '=' ^> lc)
            mylet var term body = App (Lam var body) term

op :: Parser Op
op = char '_' >> op' where
  op' = (char '+' >> return Add)
    <|> (char '-' >> return Sub)
    <|> (char '*' >> return Mul)
    <|> (char '/' >> return Div)
    <|> (char '%' >> return Mod)
    <|> (string "\\=" >> return Neq)
    <|> (char '=' >> return Eq)
    <|> try (string "<=" >> return Le)
    <|> try (string ">=" >> return Ge)
    <|> (char '<' >> return Lt)
    <|> (char '>' >> return Gt)
    <|> (char ':' >> Call <$> word <*> literal)
    <|> (char '!' >> Syscall <$> literal)
    <|> (char '@' >> Write <$> wordSizeChar)
    <|> (char '$' >> Read <$> wordSizeChar)
  wordSizeChar = (char 'b' >> return Word8)
             <|> (char 's' >> return Word16)
             <|> (char 'l' >> return Word32)
             <|> (char 'q' >> return Word64)
               
-- Useful expressions
defParse = parseSource lc
lid = defParse "\\x.x"
cons = defParse "\\h.\\t.\\n.\\c.(c h t)"
nil = defParse "\\n.\\c.n"
true = defParse "\\t.\\f.t"
false = defParse "\\t.\\f.f"
y = defParse "\\g.(\\x.[g x x] \\x.[g x x])"
pair = defParse "\\f.\\s.\\p.(p f s)"

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
  | WORLD
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
  OP (Syscall i) -> unlines$ [printf "entered_L%d:" n]
                          ++ ["movq 16(%rax), %rax"] -- World
                          ++ ["LOADVARENV %rcx"] -- Syscall
                          ++ [printf "LOADVARENV %s" reg | reg <- reverse $ take i syscallregs] -- Args
                          ++ ["movq %rcx, %rax"]
                          ++ ["syscall"]
                          ++ ["movq %rax, %rdi"]
                          ++ ["RETURNVAL ret_L" ++ show n]
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
    Write w -> "WRITE " ++ showreg w
    Read w -> "READ " ++ showreg w
  WORLD -> printf "WORLD L%d" n

showreg :: WordSize -> String
showreg w = case w of
  Word8 -> "dil"
  Word16 -> "di"
  Word32 -> "edi"
  Word64 -> "rdi"

-- This is x64 specific, need to make cross platform
syscallregs = ["%rdi", "%rsi", "%rdx", "%r10", "%r8", "%r9"]

compile :: DBExpr -> [Instr]
compile (Lam _ e) | bound 0 e == 0 = POP : compile (dec 0 e)
compile (Lam _ e)  = TAKE : compile e
compile (Var i)    = [ENTER i]
compile (App m n)  = PUSH (length ms + 1) : ms ++ compile n where ms = compile m
compile (Lit l)    = [LIT l]
compile (Op o)     = [OP o]
compile World      = [WORLD]

debugging :: SExpr -> [String]
debugging (Lam v e)  = v:debugging e
debugging (Var v)    = [v]
debugging (App m n)  = "":debugging m ++ debugging n
debugging (Lit l)    = [""]
debugging (Op o)     = [""]
debugging World      = [""]

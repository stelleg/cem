module LC where
import Control.Applicative
import Control.Monad.State
import Prelude hiding (foldl)
import qualified Data.Set as S

type DBExpr = Expr () Int
type SExpr = Expr String String

data Expr a b = Var b 
              | App (Expr a b) (Expr a b) 
              | Lam a (Expr a b)
              | Lit Literal
              | Op Op
              | World

travr :: (a -> State c (Expr a b)) 
      -> (b -> State c (Expr a b)) 
      -> Expr a b 
      -> State c (Expr a b)
travr f g (Var v) = g v
travr f g (Lam v e) = f v >> travr f g e
travr f g (App m n) = travr f g m >>= \m'-> travr f g n >>= \n' -> return $ App m' n'

travl :: (a -> State c (Expr a b)) 
      -> (b -> State c (Expr a b)) 
      -> Expr a b 
      -> State c (Expr a b)
travl f g (Var v) = g v
travl f g (Lam v e) = f v >> travl f g e
travl f g (App m n) = travl f g n >> travl f g m

type Literal = Int

data Op = Add | Sub | Mul | Div | Mod | Eq | Neq | Lt | Gt | Le | Ge | Write WordSize | Read WordSize | Call String Int | Syscall Int deriving (Eq, Ord)

data WordSize = Word8 | Word16 | Word32 | Word64 deriving (Eq, Ord)

deBruijn :: [(String, Int)] -> SExpr -> Either String DBExpr
deBruijn ls (Var v)     = maybe (Left v) (Right . Var) $ lookup v ls
deBruijn ls (Lam x t)   = Lam () <$> deBruijn ((x,0):map (fmap succ) ls) t
deBruijn ls (App t1 t2) = App <$> (deBruijn ls t1) <*> (deBruijn ls t2)
deBruijn ls (Lit l)     = Right $ Lit l
deBruijn ls (Op o)      = Right $ Op o


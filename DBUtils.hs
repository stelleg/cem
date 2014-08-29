module DBUtils where
import LC

deadCodeElim :: DBExpr -> DBExpr
deadCodeElim e = case e of
  App (Lam _ b) m | not $ elem 0 (fv b') -> dec 0 b'
                  | otherwise -> App (Lam () b') $ deadCodeElim m 
    where b' = deadCodeElim b
  Lam _ b -> Lam () $ deadCodeElim b
  e -> e

inline :: DBExpr -> DBExpr
inline e = case e of 
  App n@(Lam _ b) m | isval m && null (fv m) -> dec 0 $ replace 0 (inline m) (inline $ b)
  App n m -> App (inline n) (inline m)
  Lam _ b -> Lam () $ inline b
  e -> e

replace :: Int -> DBExpr -> DBExpr -> DBExpr
replace i m e = case e of
  Var i' | i' == i -> m
  Lam _ b -> Lam () $ replace (i+1) m b
  App n o -> App (replace i m n) (replace i m o)
  _ -> e

fv :: DBExpr -> [Int]
fv (Lam _ e) = map (flip (-) 1) $ filter (/= 0) $ fv e
fv (Var i) = [i]
fv (App m n) = fv m ++ fv n
fv (Op o) = case o of
  Add -> [0,1]
  Sub -> [0,1]
  Div -> [0,1] 
  Mul -> [0,1] 
  Mod -> [0,1] 
  Eq -> [0..3] 
  Neq -> [0..3]
  Lt -> [0..3] 
  Gt -> [0..3] 
  Le -> [0..3] 
  Ge -> [0..3] 
  Write w -> [0..2] 
  Read w -> [0..1] 
  Syscall n -> [0..n+2] 
fv _ = []  

bound :: Int -> DBExpr -> Int
bound i (Lam _ e) = bound (i+1) e
bound i (Var i') = fromEnum $ i == i'
bound i (App m n) = bound i m + bound i n
bound i (Op o) = case o of
  Add -> fromEnum $ i < 2 
  Sub -> fromEnum $ i < 2 
  Div -> fromEnum $ i < 2 
  Mul -> fromEnum $ i < 2 
  Mod -> fromEnum $ i < 2 
  Eq -> fromEnum $ i < 4
  Neq -> fromEnum $ i < 4
  Lt -> fromEnum $ i < 4
  Gt -> fromEnum $ i < 4
  Le -> fromEnum $ i < 4
  Ge -> fromEnum $ i < 4
  Write w -> fromEnum $ i < 3
  Read w -> fromEnum $ i < 2
  Syscall n -> fromEnum $ i < n + 2
bound i _ = 0

dec :: Int -> DBExpr -> DBExpr
dec i (Lam _ e) = Lam () (dec (i+1) e)
dec i (Var i') | i' == i = error "POP opt failed"
               | i' > i = Var (i'-1)
               | i' < i = Var i'
dec i (App m n) = App (dec i m) (dec i n)
dec i t = t

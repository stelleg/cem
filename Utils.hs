module Utils where
import VM
import Data.Set
import qualified LC

fvs :: LExpr -> Set String
fvs e = case e of
  Var l v -> singleton v
  Lam l v b -> delete v $ fvs b
  App l m n -> fvs m `union` fvs n
  _ -> empty

inline :: LExpr -> LExpr
inline e = inline' e where 
  inline' e = case e of 
    App l n@(Lam l' v b) m | isValue m && fvs m == empty -> replace v (inline m) (inline b)
                           | otherwise -> App l (inline n) (inline m)
    Lam l v b -> Lam l v $ inline' b
    e -> e
  replace v m e = case e of
    Var l v' -> if v' == v then m else e
    Lam l v' b -> if v' == v then e else Lam l v' $ replace v m b
    App l n o -> App l (replace v m n) (replace v m o)
    _ -> e

deadCodeElim :: LExpr -> LExpr
deadCodeElim e = case e of
  App l (Lam l' v b) m | not (member v $ fvs b') -> b' 
                       | otherwise -> App l (Lam l' v b') $ deadCodeElim m 
    where b' = deadCodeElim b
  Lam l v b -> Lam l v $ deadCodeElim b
  e -> e

opt :: LExpr -> LExpr
opt = inline . deadCodeElim

import LC
import System.Environment as SE
import Control.Monad (liftM)
import Data.Binary.Put
import VM
import Foreign.Marshal.Array (newArray, peekArray)

main = do
  code <- liftM compile.LC.parseFile .head =<< SE.getArgs
  instr <- peekArray 3 =<< c_cem =<< newArray code
  case deCompile instr of
    Lam _ (Lam _ (Var 1)) -> putStrLn "True"
    Lam _ (Lam _ (Var 0)) -> putStrLn "False"
    _ -> putStrLn "Error: not a bool!"


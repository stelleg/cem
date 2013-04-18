import LC
import System.Environment as SE
import Control.Monad (liftM)
import Data.Binary.Put
import VM
import Foreign.Marshal.Array (newArray, peekArray)

main = do
  code <- liftM compile.LC.parseFile .head =<< SE.getArgs
  print.head =<< peekArray 1 =<< c_cem =<< newArray code


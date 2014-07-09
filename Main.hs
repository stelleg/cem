import System.Environment as SE
import System.Process (system)
import Paths_cem (getDataFileName)
import Text.ParserCombinators.Parsec hiding (label)
import qualified IO
import qualified VM
import qualified Analysis as A
import LC
import Data.IORef
import Control.Applicative
import System.Exit
import qualified Data.Map as M

libs = mapM getDataFileName ["lib/prelude.lc", "lib/os.lc", "lib/church.lc"]

parseArgs :: [String] -> IO ()
parseArgs [] = usage
parseArgs (('-':o:opts):sources) = parseOpts (o:opts) sources
parseArgs sources = parseOpts "lc" sources

parseOpts :: String -> [String] -> IO ()
parseOpts ('l':o) sources = do srclibs <- libs; parseOpts o (srclibs ++ sources)
parseOpts "v" _ = version
parseOpts "h" _ = usage
parseOpts "a" sources = cfa =<< readSources sources
parseOpts "c" sources = compile (chooseOutput . last $ sources) =<< readSources sources
parseOpts "f" sources = freevars =<< readSources sources
parseOpts "g" sources = graph =<< readSources sources
parseOpts "r" sources = partial (\s->return()) =<< readSources sources 
parseOpts "t" sources = partial (\((c,e),h,s)->print c) =<< readSources sources
parseOpts _ _ = usage

chooseOutput "-" = "a.out"
chooseOutput fname = takeWhile (/= '.') fname

readSources [] = getContents
readSources s = concat <$> mapM (\f->if f == "-" then getContents else readFile f) s

version = putStrLn $ "0.1 Alpha"  
usage = putStrLn $ "Usage: cem {-lhvdgpcf} {file(s)}"

cfa s = do
  let prog = VM.labeled $ IO.parseProgram s
  putStrLn $ VM.showlabeled prog
  putStrLn $ A.ppca $ A.ca prog

graph :: String -> IO ()
graph s = do 
  ind <- newIORef 0
  VM.traceCEM (sg ind) . (\e->((e,0),(0, M.empty),[])) $ VM.labeled $ IO.parseProgram s; return ()
  where sg ind init = do modifyIORef ind (+1)
                         i <- readIORef ind
                         VM.showGraph (show i ++ ".dot") . VM.todot $ init

partial :: (VM.CEMState -> IO ()) -> String -> IO ()
partial f s = do ((c,e), h, s) <- VM.traceCEM f . (\e->((e,0),(0,M.empty),[])) $ VM.labeled $ IO.parseProgram s
                 case c of
                   VM.Lit _ (Just i) -> exitWith (ExitFailure i)
                   _ -> exitWith (ExitFailure 255)

freevars :: String -> IO ()
freevars s = print $ A.fv $ VM.labeled $ IO.parseProgram s

compile :: String -> String -> IO ()
compile filename s = do
  writeFile "/tmp/prog.lc" s
  native filename (toDeBruijn e) e where e = Lam "argc" $ Lam "argv" $ Lam "envp" $ IO.parseProgram s

toDeBruijn :: SExpr -> DBExpr
toDeBruijn expr = either (\v->error$"free var: "++v) id $ deBruijn [] expr 

native :: String -> DBExpr -> SExpr -> IO ()
native filename dbprog expr = do
  let assembly = unlines . (flip IO.toMacros (IO.debugging expr)) . IO.compile $ dbprog
  macros <- readFile =<< getDataFileName "/native/x64.s"
  gc <- readFile =<< getDataFileName "/native/gc-x64.s"
  writeFile "/tmp/prog.s" (macros ++ assembly ++ gc)
  system $ "as /tmp/prog.s -o /tmp/prog.o; ld /tmp/prog.o -o "++ filename
  return ()

main = getArgs >>= parseArgs

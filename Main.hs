{-# LANGUAGE DataKinds #-}
import Text.Printf (printf)
import System.Environment as SE
import qualified Utils as U
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
import qualified AI
import qualified ZFA as ZFA
import qualified State as S
import qualified Data.Set  as Set
import qualified DBUtils as DBU

libs = mapM getDataFileName ["lib/prelude.lc", "lib/os.lc", "lib/church.lc"]

programify b s = if b then s else "{" ++ s ++ "}" ++ "(main Ω \\v.\\w.w)"

parseArgs :: [String] -> IO ()
parseArgs [] = usage
parseArgs (('-':o:opts):sources) = parseOpts False (o:opts) sources
parseArgs sources = parseOpts False "lr" sources

parseOpts :: Bool -> String -> [String] -> IO ()
parseOpts pure ('p':o) sources = parseOpts True o sources
parseOpts pure ('l':o) sources = do srclibs <- libs; parseOpts pure o (srclibs ++ sources)
parseOpts pure "v" _ = version
parseOpts pure "h" _ = usage
parseOpts pure ('a':level) sources = readSources sources >>= \s' -> let s = programify pure s' in 
  case read level :: Int of
    1 -> (cfa s :: IO (AI.CFA 1)) >> return ()
    2 -> (cfa s :: IO (AI.CFA 2)) >> return ()
    3 -> (cfa s :: IO (AI.CFA 3)) >> return ()
    4 -> (cfa s :: IO (AI.CFA 4)) >> return ()
    5 -> (cfa s :: IO (AI.CFA 5)) >> return ()
    6 -> (cfa s :: IO (AI.CFA 6)) >> return ()
parseOpts pure "c" sources = compile (chooseOutput . last $ sources) . programify pure =<< readSources sources
parseOpts pure "f" sources = freevars . programify pure =<< readSources sources
parseOpts pure ('k':level) sources = readSources sources >>= cfak (read level) . programify pure
parseOpts pure "g" sources = graph . programify pure =<< readSources sources
parseOpts pure "r" sources = partial (\s->return()) . programify pure =<< readSources sources 
parseOpts pure "t" sources = partial (\((c,e),h,s)->putStrLn $ take 30 $ show c) . programify pure =<< readSources sources
parseOpts pure ('z':level) sources = cfaz (read level) . programify pure =<< readSources sources
parseOpts pure ('s':level) sources = readSources sources >>= case read level :: Int of
  1 -> \s -> (cfas s :: IO (S.AI 1)) >> return ()
  2 -> \s -> (cfas s :: IO (S.AI 2)) >> return ()
  3 -> \s -> (cfas s :: IO (S.AI 3)) >> return ()
  4 -> \s -> (cfas s :: IO (S.AI 4)) >> return ()
  5 -> \s -> (cfas s :: IO (S.AI 5)) >> return ()
  6 -> \s -> (cfas s :: IO (S.AI 6)) >> return ()
parseOpts _ _ _ = usage

chooseOutput "-" = "a.out"
chooseOutput ('=':prog) = "a.out"
chooseOutput fname = takeWhile (/= '.') fname

readSources [] = getContents
readSources s = concat <$> mapM chooseInput s
  where chooseInput f = case f of
                         "-" -> getContents
                         '=':p -> return p
                         _ -> readFile f

version = putStrLn $ "0.1 Alpha"  
usage = putStrLn $ "Usage: cem {-lhvdgpcf} {file(s)}"

cfaz i s = do 
  let prog = VM.labeled $ IO.parseProgram s
  let e = VM.relabeled $ U.opt prog
  putStrLn $ VM.showlabeled e
  putStrLn $ ZFA.ppca e $ ZFA.ca i e

cfas s = do
  let prog = VM.labeled $ IO.parseProgram s
  putStrLn $ VM.showlabeled prog
  let states = S.states prog 
  putStrLn $ S.ppstates states
  mapM (putStrLn . S.ppstate) $ S.getvals states [((prog, S.empty), M.empty)] Set.empty
  return states

cfak i s = do
  let prog = VM.relabeled $ VM.labeled $ IO.parseProgram s
  putStrLn $ VM.showlabeled prog
  cfa <- A.ca {-A.graphCFA-} print i prog
  let progvals = A.ca' cfa prog
  putStrLn $ "Program values: " ++ A.ppset progvals
  putStrLn "State transitions"
  putStrLn $ A.ppca cfa
  putStrLn "Summarization"
  putStrLn $ A.ppta $ A.summarize cfa

cfa s = do
  let prog = VM.labeled $ IO.parseProgram s
  putStrLn $ VM.showlabeled prog
  let (prog_vals, cfa) = AI.cfa prog
  putStrLn $ "Program values: " ++ AI.ppset prog_vals
  putStrLn $ AI.ppca $ cfa
  return (prog_vals, cfa)

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
                   VM.World _ -> exitSuccess
                   VM.Lit l (Just i) -> exitWith (ExitFailure i)
                   _ -> exitWith (ExitFailure 255)

freevars :: String -> IO ()
freevars s = print $ A.fv $ VM.labeled $ IO.parseProgram s

compile :: String -> String -> IO ()
compile filename s = do
  writeFile "/tmp/prog.lc" s
  nodebug filename (DBU.inline $ DBU.deadCodeElim $ toDeBruijn $ IO.parseProgram s)

nodebug :: String -> DBExpr -> IO ()
nodebug filename dbprog = do
  let assembly = unlines . (flip IO.toMacros $ repeat "") . IO.compile $ dbprog
  macros <- readFile =<< getDataFileName "/native/x64.s"
  gc <- readFile =<< getDataFileName "/native/gc-x64.s"
  writeFile (printf "/tmp/%s.s" filename) (macros ++ assembly ++ gc)
  system $ printf "as /tmp/%s.s -o /tmp/%s.o; ld /tmp/%s.o -o %s" filename filename filename filename
  return ()


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

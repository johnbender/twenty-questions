-- module Interactive where

import Hoogle
import Text.Regex.Posix
import Data.List
import Data.List.Split
--import System.Eval.Haskell
import GHCWrapper
import JSON
import System.Posix.Directory
import System.Environment
import Control.Applicative
import GHC (RunResult (..))

url="http://hackage.haskell.org/packages/archive/base/latest/doc/html/Data-Foldable.html#v:find"

parseResultURL url = 
  let [[_,mod,nm]] = url =~ "/html/(.*)\\.html#v:(.*)" :: [[String]]
  in (splitOn "-" mod,nm)

testResult = parseResultURL url

--hasType ty mod nm = typeOf (nm ++ " :: " ++ ty) [intercalate "." mod]
hasType ty mod nm = do
  let modStr = intercalate "." mod
  e <- runGhc [] [modStr] $ exprType (modStr ++ "." ++ nm ++ " :: " ++ ty) 
  return $ isRight e

isRight (Right _) = True
isRight _ = False

isTyped s = length s > 0

{--
partitionCandidates :: [String] -> [String] -> IO (Either [String] (Maybe JSONReport))
partitionCandidates mods nms = do
  let candidate nm = "Candidate \"" ++ nm ++ "\" " ++ nm
      candidates = intercalate "," $ map candidate nms
  cwd <- getWorkingDirectory
  eval_ ("partitionCandidates [" ++ candidates ++ "]") ("TwentyQuestions":"Candidate":mods) [] [] [cwd]


partitionCandidates :: [String] -> [String] -> IO (Maybe JSONReport)
partitionCandidates mods nms = do
  let candidate nm = "Candidate \"" ++ nm ++ "\" " ++ nm
      candidates = intercalate "," $ map candidate nms
  eval ("partitionCandidates [" ++ candidates ++ "]") ("TwentyQuestions":"Candidate":mods)
--}  

partitionCandidates :: String -> [String] -> [String] -> IO ()
partitionCandidates ty mods nms' = do
  let mods' = nub $ sort ("Prelude":mods)
      nms = nub $ sort nms'
      candidate nm = "Candidate \"" ++ nm ++ "\" " ++ nm
      candidates = intercalate "," $ map candidate nms
      stmt = "partitionCandidates (undefined :: " ++ ty ++ ") [" ++ candidates ++ "]"
  -- putStrLn stmt
  -- mapM_ putStrLn nms
  r <- runGhc ["TwentyQuestions","Candidate"] mods' $ runStmt stmt
  case r of
    Left e -> putStrLn "Error" >> print e
    Right (RunOk nm) -> return () --putStrLn "RunOk"
    Right (RunException e) -> putStrLn "RunException"
    
  return ()

  
main = do
  -- db <- loadDatabase "/home/msb/.cabal/share/hoogle-4.2.31/databases/base.hoo"
 
  -- putStrLn "Enter hoogle query: "
  -- qStr <- getLine
  
  [dbPath,qStr] <- getArgs
  db <- loadDatabase dbPath

  let Right q = parseQuery Haskell qStr
  let results = filter ((==["Prelude"]) . fst) $ map (parseResultURL . fst . head . locations . snd) $ search db q
  tys <- mapM (uncurry $ hasType qStr) results
  let typedResults = map snd $ filter fst $ zip tys results
  let modules = nub $ sort $ map (intercalate "." . fst) typedResults
      candidateNames = nub $ sort $ map (\(mods,nm) -> intercalate "." (mods ++ [nm])) typedResults
  -- mapM_ putStrLn candidateNames
  partitionCandidates qStr modules candidateNames
{-
  case out of
    (Just jsonReport) -> dumpJSONReport jsonReport
    Nothing -> putStrLn "Nothing"
-}
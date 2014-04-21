-- module Interactive where

import Text.Regex
import Text.Regex.Posix
import Data.List
import System.Posix.Directory
import System.Environment
import Control.Applicative
import Control.Monad.Trans
import Control.Monad
import GHC (RunResult (..))
import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.Syntax hiding (Generator)
import Language.Haskell.Exts.Pretty

import Generator
import JSON
import GHCWrapper
import TypeHole
import Types
import HoogleWrapper

{--
parseOk :: ParseResult a -> Maybe a
parseOk (ParseOk a) = Just a
parseOk _ = Nothing
--}

tyArity (TyFun _ t) = 1 + tyArity t
tyArity  _ = 0

uncurryTy :: Type -> Type
uncurryTy = go []
  where 
    go args (TyFun a t) = go (a:args) t
    go args t = TyFun (TyTuple Boxed $ reverse args) t

hasType ty (CandidateInfo mods expr) = typeCheck mods (expr ++ " :: " ++ ty) 

isRight (Right _) = True
isRight _ = False

isTyped s = length s > 0

-- map arity to uncurry function
uncurriers :: [(Int,String)] 
uncurriers = [(1, "id"), (2, "uncurry")]

partitionCandidates :: Type -> [CandidateInfo] -> IO ()
partitionCandidates ty cis = do
  let mods = concat $ map ciMods cis
      exprs = map ciExpr cis

  let arity = tyArity ty
      uncurriedTy = uncurryTy ty
      Just uncurrier = lookup arity uncurriers

  let mods' = nub $ sort ("Prelude":mods)
      exprs' = nub $ sort exprs
      candidate e = "Candidate \"" ++ e ++ "\" (" ++ uncurrier ++ " (" ++ e ++ "))"
      candidates = intercalate "," $ map candidate exprs'
      stmt = "partitionCandidates (undefined :: " ++ prettyPrint uncurriedTy ++ ") [" ++ candidates ++ "]"
 
  --putStrLn "Candidates: "
  --mapM_ (putStrLn . candidate) nms
  --putStrLn stmt

  r <- runGhc ["TwentyQuestions","Candidate"] mods' $ runStmt stmt
  case r of
    Left e -> putStrLn "Error" >> print e
    Right (RunOk nm) -> return () --putStrLn "RunOk"
    Right (RunException e) -> putStrLn "RunException"
  return ()
  
typeCheckCandidates :: MonadIO m => Type -> [CandidateInfo] -> m [CandidateInfo]
typeCheckCandidates t cis = do
  r <- liftIO $ runGhc [] [] (sequence $ map (hasType $ prettyPrint t) cis)
  case r of
    Left e -> fail (show e)
    Right tys -> return $ map snd $ filter fst $ zip tys cis
  
{--
runGenerator (Generator tys gen) = do
  liftIO $ do
    putStrLn $ "generating candidates for types: "
    mapM_ (putStrLn . prettyPrint) tys
  results <- mapM (findCandidates . prettyPrint) tys
  -- liftIO $ mapM_ putStrLn $ map ciExpr results
  return $ map gen (combinations results)
--}

combinations [] = [[]]
combinations (l1:ls) = [e:l | e <- l1, l <- combinations ls]

driver ty = do
  cis <- runGenerators (TypeHoleInfo ty)
  cis' <- typeCheckCandidates ty cis
  liftIO $ partitionCandidates ty cis'
  
main = do
  [dbPath] <- getArgs
  runTQM dbPath $ forever $ do
    qStr <- liftIO $ do
      putStrLn "Enter query: "
      getLine
    let Just ty = parseOk $ parseType qStr
    driver ty
    


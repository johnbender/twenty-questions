module GHCWrapper (runGhc, exprType, typeCheck, runStmt, showPpr) where

import qualified GHC as GHC
import GHC hiding (runGhc, runStmt)
import GHC.Paths (libdir)
import DynFlags
import InteractiveEval hiding (runStmt)
import Outputable hiding (showPpr)
import Control.Exception
import Exception
import Data.List (nub, sort)
import Control.Applicative
import Control.Monad.Trans

runStmt s = GHC.runStmt s RunToCompletion

defaultMods = ["Prelude"]

showPpr a = GHC.runGhcT (Just libdir) $ do
              dflags <- getSessionDynFlags
              return $ showSDocForUser dflags neverQualify (ppr a)

typeCheck :: [String] -> String -> GhcT IO Bool
typeCheck mods e = do
    let imports = nub $ sort $ defaultMods ++ mods
    result <- (gtry $ do
      setContext $ map (GHC.IIDecl . GHC.simpleImportDecl . GHC.mkModuleName) imports
      exprType e
      ) :: GhcT IO (Either SomeException Type)
    return $ isRight result

    {--}
    case result of
      Left x -> return False
      Right _ -> return True

  where isRight (Right _) = True
        isRight _ = False

runGhc :: [String] -> [String] -> GhcT IO a -> IO (Either SomeException a)
runGhc targets mods a =
  let imports = nub $ sort $ defaultMods ++ targets ++ mods in
  defaultErrorHandler defaultFatalMessager defaultFlushOut
    $ try
    $ do
    GHC.runGhcT (Just libdir) $ do
      dflags <- getSessionDynFlags
      setSessionDynFlags $ dflags {hscTarget = HscInterpreted, ghcLink = LinkInMemory , ghcMode = CompManager }
      setTargets =<< sequence (map (\t -> guessTarget t Nothing) targets)
      load LoadAllTargets
      setContext $ map (GHC.IIDecl . GHC.simpleImportDecl . GHC.mkModuleName) imports
      a

--return $ showSDoc dflags (ppr v)

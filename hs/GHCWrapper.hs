module GHCWrapper (runGhc, exprType, runStmt) where

import qualified GHC as GHC
import GHC hiding (runGhc, runStmt)
import GHC.Paths (libdir)
import DynFlags
import InteractiveEval hiding (runStmt)
import Outputable
import Control.Exception

runStmt s = GHC.runStmt s RunToCompletion

runGhc :: [String] -> [String] -> Ghc a -> IO (Either SomeException a)
runGhc targets mods a = 
  -- defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
  defaultErrorHandler defaultFatalMessager defaultFlushOut 
    $ try
    $ do
    GHC.runGhc (Just libdir) $ do
      dflags <- getSessionDynFlags
      setSessionDynFlags $ dflags {hscTarget = HscInterpreted, ghcLink = LinkInMemory , ghcMode = CompManager }
      setTargets =<< sequence (map (\t -> guessTarget t Nothing) targets)
      load LoadAllTargets
      setContext $ map (GHC.IIDecl . GHC.simpleImportDecl . GHC.mkModuleName) (targets ++ mods)

      a
--return $ showSDoc dflags (ppr v)
 

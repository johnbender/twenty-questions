module Types where

import Hoogle (Database)
import Language.Haskell.Exts.Syntax (Type)
import Control.Monad.Reader

type TQM = ReaderT Database IO

-- Heuristics for generating expressions based on the type
data CandidateInfo = CandidateInfo { ciMods :: [String], ciExpr :: String }

-- genTypes are subqueries to hoogle to find potential subexpressions
-- generator builds a candidate from candidates for each subexpression
data Generator = Generator { genTypes :: [Type], 
                             generator :: [CandidateInfo] -> TQM [CandidateInfo] }

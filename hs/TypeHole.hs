module TypeHole where

import Control.Applicative
import Data.Maybe
import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.Syntax
import Language.Haskell.Exts.Pretty
import Text.Regex
import Text.Regex.Posix
import Control.Arrow

import GHCWrapper

data TypeHoleInfo = TypeHoleInfo { typeHoleType :: Type }
                  | NamedTypeHoleInfo { namedTypes :: [(String,Type)] }

instance Show TypeHoleInfo where
  show (TypeHoleInfo t) = "TypeHoleInfo " ++ prettyPrint t
  show (NamedTypeHoleInfo ts) = "NamedTypeHoleInfo " ++ show (map (second prettyPrint) ts)

parseOk :: ParseResult a -> Maybe a
parseOk (ParseOk a) = Just a
parseOk _ = Nothing

replace x y = go
  where len = length x
        go l | x == take len l = y ++ go (drop len l)
        go (c:l) = c : go l
        go [] = []

parseTypeHoleInfo :: [String] -> String -> String -> IO (TypeHoleInfo)
parseTypeHoleInfo mods expr ty = do
  Left e <- runGhc [] mods (exprType $ "(" ++ expr  ++ ") :: " ++ ty)
  let msg = replace "GHC.Types." "" $ concat $ lines $ show e
  --let msg = concat $ lines $ show e
  let matches = msg =~ "Found hole ._(.*).  with type: (.*)" :: [[String]]
      parseMatch [_,nm,strTy] = do ty <- parseOk (parse strTy)
                                   return (nm,ty)
  print msg
  print matches
  return $ NamedTypeHoleInfo $ catMaybes $ map parseMatch matches

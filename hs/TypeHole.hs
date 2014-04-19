module TypeHole where

import Control.Applicative
import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.Syntax
import Language.Haskell.Exts.Pretty
import Text.Regex
import Text.Regex.Posix

import GHCWrapper

data TypeHoleInfo = TypeHoleInfo { typeHoleType :: Type }

parseOk :: ParseResult a -> Maybe a
parseOk (ParseOk a) = Just a
parseOk _ = Nothing

replace x y = go
  where len = length x
        go l | x == take len l = y ++ go (drop len l)
        go (c:l) = c : go l
        go [] = []

parseTypeHoleInfo :: [String] -> String -> String -> IO (Maybe TypeHoleInfo)
parseTypeHoleInfo mods expr ty = do
  Left e <- runGhc [] mods (exprType $ "(" ++ expr  ++ ") :: " ++ ty)
  let msg = replace "GHC.Types." "" $ concat $ lines $ show e
  --let msg = concat $ lines $ show e
  let [[_,strTy]] = msg =~ "Found hole .* with type: (.*)" :: [[String]]
  return $ TypeHoleInfo <$> parseOk (parse strTy)

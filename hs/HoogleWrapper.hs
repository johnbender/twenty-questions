module HoogleWrapper where

import Types

import Hoogle hiding (search)
import qualified Hoogle
import Control.Monad.Reader
import Control.Applicative
import Data.List
import Text.Regex
import Text.Regex.Posix
import Data.List.Split
import Data.Char

badCandNames = [
    "Prelude.toEnum"
  , "Prelude.asTypeOf"
  , "Control.Monad.forever"
  , "Prelude.cycle"
  , "Prelude.id"
  , "Prelude.fromEnum"
  , "Prelude.fromIntegral"
  ]

filterCands = filter (\c -> not $ elem (ciExpr c) badCandNames)

runTQM dbPath act = do
  db <- loadDatabase dbPath
  runReaderT act db

search :: Query -> TQM [(Score, Result)]
search q = do
  db <- ask
  return $ Hoogle.search db q

hoogleFlags = "-Unsafe -Data.Bits -Foreign -Data.Trace -GHC -Control.DeepSeq -Data.Generics -Debug.Trace "

findCandidates :: String -> TQM [CandidateInfo]
findCandidates qStr = do
  let qStr' = hoogleFlags ++ qStr
  case parseQuery Haskell qStr' of
     Right q -> do results <- map (parseResultURL . fst . head . locations . snd) <$> search q
                   return $ filterCands $ map (\(mod,nm) -> CandidateInfo [mod] (mod ++ "." ++ nm)) results
     Left _ -> do liftIO . putStrLn $ "findCandidates: could not parse query " ++ qStr'
                  return []
                        

parseResultURL url = 
  let [[_,mod,nm]] = url =~ "/html/(.*)\\.html#v:(.*)" :: [[String]]
  in (intercalate "." $ splitOn "-" mod, asciiDecode nm)
  
asciiDecode = replaceAll "-([0-9]+)-" (pure . chr . read . init . tail)

replaceAll :: String -> (String -> String) -> String -> String
replaceAll re f s = start end
  where (_, end, start) = foldl' go (0, s, id) $ getAllMatches $ match (mkRegex re) s
        go (ind,read,write) (off,len) =
          let (skip, start) = splitAt (off - ind) read 
              (matched, remaining) = splitAt len start 
          in (off + len, remaining, write . (skip++) . (f matched ++))

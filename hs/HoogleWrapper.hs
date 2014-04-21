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



runTQM dbPath act = do
  db <- loadDatabase dbPath
  runReaderT act db

search :: Query -> TQM [(Score, Result)]
search q = do
  db <- ask
  return $ Hoogle.search db q

findCandidates :: String -> TQM [CandidateInfo]
findCandidates qStr = do
  case parseQuery Haskell qStr of
     Right q -> do results <- map (parseResultURL . fst . head . locations . snd) <$> search q
                   return $ map (\(mod,nm) -> CandidateInfo [mod] (mod ++ "." ++ nm)) results
     Left _ -> do liftIO . putStrLn $ "findCandidates: could not parse query " ++ qStr
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

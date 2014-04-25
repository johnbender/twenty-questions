import Hoogle
import System.Environment
import Control.Applicative

main = do [outDb] <- getArgs
          inDbs <- lines <$> getContents
          mergeDatabase inDbs outDb 
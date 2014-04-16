import Control.Arrow
import Data.List
import qualified Data.ByteString.Lazy.Char8 as BS

import Candidate
import JSON

candidates = [
    Candidate "head" head
  , Candidate "last" last
  , Candidate "maximum" maximum
  , Candidate "minimum" minimum
  , Candidate "sum" sum
  , Candidate "length" length
  , Candidate "product" product
  ]

reports = do
  mapM (runReport candidates) [[],[0],[1],[2],[4,5]]

  
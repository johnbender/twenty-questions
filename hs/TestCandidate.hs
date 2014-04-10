import Candidate

import Data.List
import Control.Arrow

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

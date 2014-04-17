{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}
module JSON  where

import Control.Arrow
import Data.Aeson
import Data.Typeable
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.List (sortBy)
import Data.Function (on)
import GHC.Generics

import Candidate

data JSONReport = JSONReport { input :: String, candidateGroups :: [CandidateGroup] }
  deriving (Show,Generic,Typeable)

data CandidateGroup = CandidateGroup { output :: String, candidates :: [String] }
  deriving (Show,Generic)

instance ToJSON JSONReport
instance ToJSON CandidateGroup

report2JSONReport r = JSONReport (show $ repInput r) groups
  where groups = map (uncurry CandidateGroup) . map (second $ map candidateName) $ map (first show ) $ repAssocs r

reportToJSON :: (Show a, Show b) => Report a b -> ByteString
reportToJSON = encode . report2JSONReport

dumpReport :: (Show a, Show b) => Report a b -> IO ()
dumpReport = dumpJSONReport . report2JSONReport

dumpJSONReport :: JSONReport -> IO ()
dumpJSONReport = BS.putStrLn . encode
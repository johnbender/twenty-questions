{-# LANGUAGE DeriveGeneric #-}
module JSON (reportToJSON) where

import Control.Arrow
import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Data.List (sortBy)
import Data.Function (on)
import GHC.Generics

import Candidate

data JSONReport = JSONReport { input :: String, candidateGroups :: [CandidateGroup] }
  deriving (Show,Generic)

data CandidateGroup = CandidateGroup { output :: String, candidate :: [String] }
  deriving (Show,Generic)

instance ToJSON JSONReport
instance ToJSON CandidateGroup

{--
report2JSONReport r = JSONReport (show $ repInput r) groups
  where groups = toCandidateGroup $ if errors then groupByError else groupByValue
        toCandidateGroup = map (uncurry CandidateGroup) . map (second $ map candidateName) 
        groupByValue = map (first show ) $ repAssocs r
        groupByError = mergeErrors $ sortBy (compare `on` fst) $ map (first showError) $ repAssocs r
        errors = any (isLeft.fst) $ repAssocs r
        showError (Left _) = "Error"
        showError _ = "Ok"
        mergeErrors ((o1,l1):(o2,l2):xs) | o1 == o2 = mergeErrors $ (o1,l1 ++ l2) : xs
                                         | otherwise = (o1,l1) : mergeErrors ((o2,l2):xs)
        mergeErrors xs = xs
--}

report2JSONReport r = JSONReport (show $ repInput r) groups
  where groups = map (uncurry CandidateGroup) . map (second $ map candidateName) $ map (first show ) $ repAssocs r


reportToJSON :: (Show a, Show b) => Report a b -> ByteString
reportToJSON = encode . report2JSONReport
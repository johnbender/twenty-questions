{-# LANGUAGE ScopedTypeVariables #-}
module Candidate where

import Control.Exception
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Data.List
import Data.Ord (comparing)
import Data.Function (on)
import Data.Data
import Data.Generics.Schemes (gsize)
import Math.Statistics

import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.MultiMap as MM

-- TODO: polymorphic candidates?
data Candidate a b = Candidate String (a -> b)

instance Show (Candidate a b) where
  show (Candidate l _) = l

instance Eq (Candidate a b) where
  (Candidate l1 _) == (Candidate l2 _) = l1 == l2

type CandidateSet a b = S.Set (Candidate a b)

type Output b = Either String b

data Report a b = Report { repInput :: a, repResults :: MM.MultiMap (Output b) (Candidate a b) }

data Score = Score { scoreNumOutputs :: Int, 
                     scoreInputSize :: Int, 
                     scoreAvgOutputSize :: Double,
                     scorePartitionSizeStdDev :: Double
                   }
  deriving (Show, Eq, Ord)

repOutputs = MM.keys . repResults

repAssocs = MM.assocs . repResults

repOutputCandidates out = MM.lookup out . repResults

repScore :: (Data a, Data b) => Report a b -> Score
repScore r@(Report input results) = Score numOutputs (gsize input) (fromIntegral (sum (map outSize $ MM.keys results)) / fromIntegral numOutputs) (repPartitionSizeStdDev r)
  where numOutputs = repNumOutputs r
        outSize (Left _) = 0
        outSize (Right o) = gsize o
        
isLeft (Left _) = True
isLeft _ = False

repNumOutputs rep =
  let l = repAssocs rep
      numCandidates = length . snd
      (errors,nonErrors) = partition (\ (o,_) -> isLeft o) l
  in if not $ null errors
     then 2
     else length l

repPartitionSizeStdDev :: Report a b -> Double
repPartitionSizeStdDev rep = 
  let l = repAssocs rep
      hasErrors = not $ null errors
      (errors,nonErrors) = partition (\ (o,_) -> isLeft o) l
      numCandidates = length . snd
      avg l = fromIntegral (sum l) / fromIntegral (length l)
  in if hasErrors then (stddev $ map fromIntegral [sum $ map numCandidates errors, sum $ map numCandidates nonErrors]) * 2
                  else (stddev $ map (fromIntegral . numCandidates) l) * fromIntegral (length l)


getOutput :: (a -> b) -> a -> IO (Output b)
getOutput f a = 
  let o = f a
  in evaluate (o `seq` Right o) `catch` (\ (e :: SomeException) -> return $ Left "error")

runReport :: Ord b => [Candidate a b] -> a -> IO (Report a b)
runReport candidates input = do
  outputs <- mapM (\ (Candidate _ f) -> getOutput f input) candidates
  let results = MM.fromList $ zip outputs candidates
  return $ Report input results

memoSortBy :: Ord b => (a -> b) -> [a] -> [a]
memoSortBy f = map snd . sortBy (comparing fst) . map (\r -> (f r, r))

sortReports = memoSortBy repPartitionSizeStdDev . filter ((/=1) . length . repOutputs)

bestReport = head . sortReports

genReports :: (Arbitrary a, Ord b) => Int -> [Candidate a b] -> IO [Report a b]
genReports nInputs candidates = do
  inputs <- sample' arbitrary
  mapM (runReport candidates) (take nInputs inputs)

-- is input an algebraic data type
-- special case: don't consider strings or tuples to be ADTs.
isADT :: Data a => a -> Bool
isADT a | dataTypeName t == "Prelude.[]" = False
        | isAlgType t = length (dataTypeConstrs t) > 1
        | otherwise = False
  where t = dataTypeOf a

groupByConstr :: Data a => [a] -> [[a]]
groupByConstr = groupBy ((==) `on` toConstr)
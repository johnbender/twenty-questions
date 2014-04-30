module TwentyQuestions where

import Candidate
import JSON
import Data.Data
import Test.QuickCheck
import Text.PrettyPrint.Boxes
import System.IO

repBox :: (Show a, Show b) => Report a b -> Box
repBox rep =
  let input = text . show $ repInput rep
      assocs = repAssocs rep
      boxes = map (uncurry $ mkOutputBox input) $ zip [0..] assocs
      outputsBox = punctuateH left (text "   ") boxes
      labels = vcat left $ map text ["Number: ", "Output: ", "Candidates: "]
  in labels <> outputsBox

mkOutputBox input num (output, candidates) =
  let numBox = text $ show num
      o = text $ case output of
                    Left OK -> "OK"
                    Left _ -> "Error"
                    Right v -> show v
      io = input <> text " -> " <> o
      cnames = map (text . candidateName) candidates
  in vcat left (numBox:io:cnames)


partitionCandidates :: (Arbitrary a, Show a, Data a, Show b, Data b, Ord b) => (a -> b) -> [Candidate a b] -> IO ()
partitionCandidates _ [] = putStrLn "No candidates."
partitionCandidates _ [c] = putStrLn $ "Done: " ++ candidateName c
partitionCandidates f cs = do
  rs <- genReports 1000 cs
  let rs' = sortReports rs
  let r = bestReport rs
  final <- shrinkReport r cs

-- show report
  printBox $ repBox final

  -- prompt for input
  putStrLn ""
  putStr "enter candidate set number: "
  hFlush stdout
  s <- getLine
  putStrLn ""

  partitionCandidates f (snd (repAssocs r !! read s))

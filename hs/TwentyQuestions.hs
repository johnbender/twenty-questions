module TwentyQuestions where

import Candidate
import JSON
import Data.Data
import Test.QuickCheck
import Text.PrettyPrint.Boxes

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
partitionCandidates _ [c] = putStrLn "Done."
partitionCandidates f cs = do
  rs <- genReports 1000 cs
  let rs' = sortReports rs
  -- mapM_ (\r -> print (repScore r) >> print (repAssocs r)) rs'
  let r = bestReport rs
  --dumpReport r
  printBox $ repBox r
  putStrLn "enter candidate set number"
  s <- getLine
  partitionCandidates f (snd (repAssocs r !! read s))
  --return $ report2JSONReport r
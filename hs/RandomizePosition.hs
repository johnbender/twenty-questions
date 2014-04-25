import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Data.Tuple

randomizeFst :: (Gen a, b) -> IO [(a, b)]
randomizeFst (a, x) = do
  as <- sample' a
  return $ zip as $ repeat x


randomizeSnd :: (a, Gen b) -> IO [(a, b)]
randomizeSnd pair = do
  as <- randomizeFst $ swap pair
  return $ map swap as

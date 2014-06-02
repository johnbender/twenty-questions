{-# LANGUAGE DeriveDataTypeable #-}

import Data.List
import Data.Ord
import Data.Function
import Data.Char
import Data.Generics.Uniplate.Data
import Data.Data

import Control.Applicative
import Control.Monad.Random hiding (split)

{-- 

This file demonstrates the idea of using Twenty Questions to derive a
parser by example.

We start with a data declaration T, which Twenty Questions uses to ask
for the bootstrapping examples: one simple example to show how each
case is parsed.  We can also use these to define a pretty-printer, pr.

The initial parser is ambiguous, which is made evident by the fact
that multiple T objects are printed as the same string.  We can detect
these ambiguities using rand and gen, which systematically generate T
objects of a certain height.  We can then check if any two are printed
to the same string.

If we find two T-values t1 and t2 such that s == pr t1 == pr t2, we
have detected an ambiguity.  To resolve it, we can ask the user which
is the correct parse of s.  We use the answers to these questions to
define valid, which restricts the domain of pr.

Our parser is naive: it generates many "pre-parses", and filters out
the invalid ones.  If our printer is injective, then our parser is
unambiguous, and there will be at most one valid parse.

Future work: generating an efficient parser, reporting parse errors

 --}

data T = N Int | Paren T | Add T T | Sub T T | Mul T T | Div T T
  deriving (Eq, Ord, Show, Data, Typeable)

-- pr: right inverse of parser (when domain is restricted to valid Ts)
-- generated from bootstrapping examples
pr (N i) = show i
pr (Paren t) = "(" ++ pr t ++ ")"
pr (Add a b) = pr a ++"+"++ pr b
pr (Sub a b) = pr a ++"-"++ pr b
pr (Mul a b) = pr a ++"*"++ pr b
pr (Div a b) = pr a ++"/"++ pr b

-- random generation
rand2 d = do a <- rand d
             b <- rand d
             return (a,b)

--rand 0 = do N <$> getRandomR (0,1000)
rand 0 = return (N 0)
rand d = do let d' = d-1
            r <- getRandomR (1,5)
            let _ = r :: Int
            case r of
              1 -> Paren <$> rand d'
              2 -> uncurry Add <$> rand2 d'
              3 -> uncurry Sub <$> rand2 d'
              4 -> uncurry Mul <$> rand2 d'
              5 -> uncurry Div <$> rand2 d'
 
-- systematic generation
gen :: Int -> [T]
gen 0 = [N 0]
gen d = [N 0] ++
        [Paren t | t <- gen (d - 1)] ++
        [t | a <- gen (d-1)
           , b <- gen (d-1)
           , c <- [Add,Sub,Mul,Div]
           , let t = c a b
           , valid' t]


-- establish associativity 
valid' (Add _ (Add _ _)) = False
valid' (Sub _ (Sub _ _)) = False
valid' (Mul _ (Mul _ _)) = False
valid' (Div _ (Div _ _)) = False

-- establish precedence
valid' (Add _ (Sub _ _)) = False
valid' (Sub _ (Add _ _)) = False

valid' (Mul (Add _ _) _) = False
valid' (Mul (Sub _ _) _) = False
valid' (Mul _ (Add _ _)) = False
valid' (Mul _ (Sub _ _)) = False
valid' (Mul _ (Div _ _)) = False

valid' (Div (Add _ _) _) = False
valid' (Div (Sub _ _) _) = False
valid' (Div _ (Add _ _)) = False
valid' (Div _ (Sub _ _)) = False
valid' (Div _ (Mul _ _)) = False

valid' _ = True

valid = all valid' . universe  


detectAmbig :: [T] -> [(String,[T])]
detectAmbig ps =
  let ps' = nub $ sort ps
      prPs = [(pr p,p) | p <- ps']
      group' = groupBy ((==) `on` fst)
      sort' = sortBy (comparing fst)
      flatten l = (fst $ head l, map snd l)
  in map flatten $ filter (\l -> length l > 1) $ group' $ sort' prPs

findOccs f s = [i | (i,c) <- zip [0..] s
                  , f c == True ]

split i s = [[s!!i], take i s, drop (i+1) s]

isOp c = c `elem` "+-/*"

parses s | head s' == '(' && last s' == ')' = map Paren $ parses (init $ tail s')
  where s' = trim s
parses s | null $ findOccs isOp s = parses' [s]
parses s = [p | i <- findOccs isOp s
              , p <- parses' $ split i s
              , valid p
              , pr p == s]

op "+" = Add
op "-" = Sub
op "*" = Mul
op "/" = Div

trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

parses' [s] | head s' == '(' && last s' == ')' = map Paren $ parses (init $ tail s')
  where s' = trim s

parses' [s] | all isDigit s = [N $ read s]
parses' [a,b,c] = [op a b' c' | b' <- parses b, c' <- parses c]
parses' _ = []


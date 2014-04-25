module Generator where

import Types
import TypeHole
import HoogleWrapper
import GHCWrapper
import Language.Haskell.Exts.Syntax hiding (Generator)
import Language.Haskell.Exts.Pretty
import Control.Monad.Trans
import Control.Applicative


--findGenerators info = concat $ map (\f -> f info) generators

runGenerators info = concat <$> mapM (\gen -> gen info) generators

generators = [simpleGen, foldl1Gen, mapGen, filterGen, fstGen, sndGen, firstGen, secondGen, uncurryGen]

simpleGen :: TypeHoleInfo -> TQM [CandidateInfo]
simpleGen = findCandidates . prettyPrint . typeHoleType

foldl1Gen :: TypeHoleInfo -> TQM [CandidateInfo]
foldl1Gen info = 
  let mkCand ci = ci { ciExpr = "(Prelude.foldl1 (" ++ ciExpr ci ++ "))" }
  in
  case typeHoleType info of
    -- [T] -> T
    TyFun (TyList t1) t2 | t1 == t2 -> do
      cands <- findCandidates (prettyPrint $ t1 `TyFun` (t1 `TyFun` t1))
      return $ map mkCand cands
    _ -> return []

filterGen :: TypeHoleInfo -> TQM [CandidateInfo]
filterGen info = 
  let mkCand ci = ci { ciExpr = "(Prelude.filter (" ++ ciExpr ci ++ "))" }
  in
  case typeHoleType info of
    -- [T] -> T
    TyFun (TyList t1) (TyList t2) | t1 == t2 -> do
      cands <- findCandidates (prettyPrint t1 ++ " -> Bool")
      return $ map mkCand cands
    _ -> return []


mapGen :: TypeHoleInfo -> TQM [CandidateInfo]
mapGen info = 
  let mkCand ci = ci { ciExpr = "(Prelude.map (" ++ ciExpr ci ++ "))" }
  in
  case typeHoleType info of
    -- [T] -> T
    TyFun (TyList t1) (TyList t2) -> do
      cands <- findCandidates (prettyPrint $ t1 `TyFun` t2)
      return $ map mkCand cands
    _ -> return []


pairGen :: (Type -> Type -> Type -> TQM [CandidateInfo]) -> TypeHoleInfo -> TQM [CandidateInfo]
pairGen fn info =
  case typeHoleType info of
    TyFun (TyTuple _ [t1,t2]) t3 -> fn t1 t2 t3
    _ -> return []

fstGen = pairGen $ \t1 _ t3 -> do
  let mkCand ci = ci { ciExpr = "((Prelude..) " ++ ciExpr ci ++ " (Prelude.fst))" }
  map mkCand <$> (runGenerators $ TypeHoleInfo (TyFun t1 t3))

sndGen = pairGen $ \_ t2 t3 -> do
  let mkCand ci = ci { ciExpr = "((Prelude..) " ++ ciExpr ci ++ " (Prelude.snd))" }
  map mkCand <$> (runGenerators $ TypeHoleInfo (TyFun t2 t3))

firstGen = pairGen $ \t1 t2 t3 ->
  case t3 of
    TyTuple _ [t4,t5] -> do
      let mkCand ci = ci { ciMods = "Control.Arrow" : ciMods ci
                         , ciExpr = "((Control.Arrow.first) (" ++ ciExpr ci ++ "))"
                         }
      map mkCand <$> (runGenerators $ TypeHoleInfo (TyFun t1 t4))
    _ -> return []

secondGen = pairGen $ \t1 t2 t3 ->
  case t3 of
    TyTuple _ [t4,t5] -> do
      let mkCand ci = ci { ciMods = "Control.Arrow" : ciMods ci
                         , ciExpr = "((Control.Arrow.second) (" ++ ciExpr ci ++ "))"
                         }
      map mkCand <$> (runGenerators $ TypeHoleInfo (TyFun t2 t5))
    _ -> return []


uncurryGen = pairGen $ \t1 t2 t3 -> do
  let mkCand ci = ci { ciExpr = "((Prelude.uncurry) " ++ ciExpr ci ++ ")" }
  map mkCand <$> (runGenerators $ TypeHoleInfo (TyFun t1 (TyFun t2 t3)))


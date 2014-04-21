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

generators = [simpleGen, foldl1Gen, fstGen]

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

pairGen :: (Type -> Type -> Type -> TQM [CandidateInfo]) -> TypeHoleInfo -> TQM [CandidateInfo]
pairGen fn info =
  case typeHoleType info of
    TyFun (TyTuple _ [t1,t2]) t3 -> fn t1 t2 t3
    _ -> return []

fstGen = pairGen $ \t1 _ t3 -> do
  let mkCand ci = ci { ciExpr = "((Prelude..) " ++ ciExpr ci ++ " (Prelude.fst))" }
  map mkCand <$> (findCandidates $ prettyPrint (TyFun t1 t3))

sndGen = pairGen $ \_ t2 t3 -> do
  let mkCand ci = ci { ciExpr = "((Prelude..) " ++ ciExpr ci ++ " (Prelude.snd))" }
  map mkCand <$> (findCandidates $ prettyPrint (TyFun t2 t3))

uncurryGen = pairGen $ \t1 t2 t3 -> do
  let mkCand ci = ci { ciExpr = "((Prelude.uncurry) " ++ ciExpr ci ++ ")" }
  map mkCand <$> (findCandidates $ prettyPrint (TyFun t1 (TyFun t2 t3)))

{--
composeGen :: TypeHoleInfo -> TQM [CandidateInfo]
composeGen info =
  let t@(TyFun a c) = typeHoleType info
      b = TyVar (Ident "x")
      cont [ci] = do
        let mods = ciMods ci
            fillHole f = "((Prelude..) (" ++ ciExpr ci ++ ") (" ++ f ++ "))" 
            expr = fillHole "_f"
            mkCI ci' = CandidateInfo (ciMods ci ++ ciMods ci') (fillHole $ ciExpr ci')
        liftIO . putStrLn $ "composeGen: " ++ expr
        NamedTypeHoleInfo holes <- liftIO $ parseTypeHoleInfo mods expr (prettyPrint t)
        liftIO $ print holes
        case lookup "f" holes of
             Nothing -> do liftIO . putStrLn $ "strange: " ++ show holes
                           return []
             Just f  -> do cis <- findCandidates (prettyPrint f)
                           return $ map mkCI cis
  in [Generator [TyFun b c] cont]
       
--}
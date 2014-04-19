module Generator where

import TypeHole

import Language.Haskell.Exts.Syntax hiding (Generator)

-- Heuristics for generating expressions based on the type

data Generator = Generator { genType :: Type, generator :: String -> String }

findGenerators info = concat $ map (\f -> f info) generators

generators = [foldl1Gen]

foldl1Gen :: TypeHoleInfo -> [Generator]
foldl1Gen info = 
  case typeHoleType info of
    -- [T] -> T
    TyFun (TyList t1) t2 | t1 == t2 -> [Generator (t1 `TyFun` t1 `TyFun` t1) (\match -> "(foldl1 " ++ match ++ ")")]
    _ -> []

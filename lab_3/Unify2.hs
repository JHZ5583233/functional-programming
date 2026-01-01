module Unify2 (mgu, applyUnifier) where

import  Types

mgu :: FuncApplication -> FuncApplication -> Maybe Unifier
mgu (FuncApp p1 as1) (FuncApp p2 as2)
    | p1 /= p2 = Nothing
    | length as1 /= length as2 = Nothing
    | null rs = Nothing
    | otherwise = Just cs
        where
            zs = zip as1 as2
            rs = reduceConstConst zs
            cs = correctType rs

argName :: Argument -> String
argName (Arg s)   = s
argName (Const s) = s

reduceConstConst :: [(Argument, Argument)] -> [(Argument, Argument)]
reduceConstConst [] = []
reduceConstConst ((Const _, Const _):as) = reduceConstConst as
reduceConstConst (a:as) = a : reduceConstConst as

correctType :: [(Argument, Argument)] -> [(String, Argument)]
correctType [] = []
correctType ((c@(Const _), a):rs) = (argName a, c) : correctType rs
correctType ((a1, a2):as) = (argName a1, a2) : correctType as

applyUnifier :: Unifier -> FuncApplication -> FuncApplication
applyUnifier us (FuncApp cs as) = FuncApp cs pas
    where
        pas = subArgs us as

subArgs :: Unifier -> [Argument] -> [Argument]
subArgs _ [] = []
subArgs us (Const cs:as) = Const cs : subArgs us as
subArgs us (Arg cs:as) = c : subArgs us as
    where
        c = getConst cs us

getConst :: String -> Unifier -> Argument
getConst ss [] = Arg ss
getConst ss ((cs, a):us)
    | ss == cs = a
    | otherwise = getConst ss us
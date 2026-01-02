module Unify (mgu, applyUnifier) where

import  Types
import Types (Argument)

mgu :: FuncApplication -> FuncApplication -> Maybe Unifier
mgu (FuncApp p1 as1) (FuncApp p2 as2)
    | p1 /= p2 = Nothing
    | length as1 /= length as2 = Nothing
    | any incorrectAssignment zs = Nothing
    | null rs = Just []
    | otherwise = Just cs
        where
            zs = zip as1 as2
            rs = reduceConstConst zs
            cs = correctType rs

argName :: Argument -> String
argName (Arg s)   = s
argName (Const s) = s

incorrectAssignment :: (Argument, Argument) -> Bool
incorrectAssignment (Const a1, Const a2) = a1 /= a2
incorrectAssignment _ = False

reduceConstConst :: [(Argument, Argument)] -> [(Argument, Argument)]
reduceConstConst [] = []
reduceConstConst ((Const _, Const _):as) = reduceConstConst as
reduceConstConst (a:as) = a : reduceConstConst as

correctType :: [(Argument, Argument)] -> [(String, Argument)]
correctType [] = []
correctType ((c@(Const _), a):rs) = (argName a, c) : correctType rs
correctType ((a1, a2):as) = (argName a1, a2) : correctType as

unify :: [(Argument, Argument)] -> [(Argument, Argument)] -> [(Argument, Argument)]
unify [] (a:as) = unify [a] as
unify rs [] = rs

indUnify :: [(Argument, Argument)] -> (Argument, Argument) -> [(Argument, Argument)]

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
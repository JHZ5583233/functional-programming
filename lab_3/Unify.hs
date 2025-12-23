module Unify (mgu, applyUnifier, reduceArg) where

import  Types
import Data.List (reverse)

mgu :: FuncApplication -> FuncApplication -> Maybe Unifier
mgu (FuncApp p1 as1) (FuncApp p2 as2)
    | p1 /= p2 = Nothing
    | length as1 /= length as2 = Nothing
    | reduceHelp zs = Nothing
    | otherwise = Just as
        where
            zs = zip as1 as2
            rs = reduceArg zs
            czs = correctType (correctWay rs)
            as = correctAmount [] (reverse czs)

reduceArg :: [(Argument, Argument)] -> [(Argument, Argument)]
reduceArg [] = []
reduceArg (c@(Const x, Const y):rs)
    | x == y = reduceArg rs
    | otherwise = c : reduceArg rs
reduceArg (r:rs) = r:reduceArg rs

reduceHelp :: [(Argument, Argument)] -> Bool
reduceHelp [] = False
reduceHelp ((Const x, Const y):rs)
    | x == y = reduceHelp rs
    | otherwise = True
reduceHelp (_:rs) = reduceHelp rs

correctWay :: [(Argument, Argument)] -> [(Argument, Argument)]
correctWay ((x, y):rs)
    | isConstArg (x, y) = (y, x) : correctWay rs
    | otherwise         = (x, y) : correctWay rs
        where
            isConstArg (Const _, Arg _) = True
            isConstArg _                = False
correctWay [] = []

correctType :: [(Argument, Argument)] -> [(String, Argument)]
correctType [] = []
correctType ((Arg cs, a):rs) = (cs, a) : correctType rs
correctType ((Const cs, a):rs) = (cs, a) : correctType rs

correctAmount :: [(String, Argument)] -> [(String, Argument)] -> [(String, Argument)]
correctAmount rs [] = rs
correctAmount [] (a:as) = correctAmount [a] as
correctAmount rs (a@(cs, Const ccs):as)
    | any f rs = correctAmount rs as
    | all i rs = correctAmount (a:rs) as
    | otherwise = []
        where
            f = sameSus a
            i = invalidAssignment a
correctAmount rs (a@(cs, Arg ccs):as)
    | any f rs = mergeArg rs a
    | all i rs = correctAmount (a:rs) as
    | otherwise = []
        where
            f = canMerge a
            i = invalidAssignment a

sameSus :: (String, Argument) -> (String, Argument) -> Bool
sameSus (c1s, a1s) (c2s, a2s)
    | c1s /= c2s = False
    | argName a1s /= argName a2s = False
    | otherwise = True

canMerge :: (String, Argument) -> (String, Argument) -> Bool
canMerge (c1s, _) (c2s, _)
    | c1s == c2s = True
    | otherwise = False

argName :: Argument -> String
argName (Arg s)   = s
argName (Const s) = s

mergeArg :: [(String, Argument)] -> (String, Argument) -> [(String, Argument)]
mergeArg [] sa = [sa]
mergeArg (sa@(c1s, a1):sas) s2a@(c2s, a2)
    | c1s == c2s = sa: (argName a2, a1) : sas
    | otherwise = sa : mergeArg sas s2a

invalidAssignment :: (String, Argument) -> (String, Argument) -> Bool
invalidAssignment (c1s, a1s) (c2s, a2s)
    | c1s == c2s && argName a1s /= argName a2s = False
    | otherwise = True

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
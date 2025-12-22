module Unify (mgu, applyUnifier, reduceArg) where

import  Types

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
            as = correctAmount [] czs

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
correctAmount rs (a:as)
    | any f rs = correctAmount rs as
    | all i rs = correctAmount (a:rs) as
    | otherwise = []
        where
            f = sameSus a
            i = invalidAssignment a

-- make two different same sus one for arg and one for const
sameSus :: (String, Argument) -> (String, Argument) -> Bool
sameSus (c1s, Const a1s) (c2s, Const a2s)
    | c1s /= c2s = False
    | a1s /= a2s = False
    | otherwise = True
sameSus (c1s, Arg a1s) (c2s, Arg a2s)
    | c1s /= c2s = False
    | a1s /= a2s = False
    | otherwise = True

invalidAssignment :: (String, Argument) -> (String, Argument) -> Bool
invalidAssignment (c1s, Const a1s) (c2s, Const a2s)
    | c1s == c2s && a1s /= a2s = False
    | otherwise = True
invalidAssignment (c1s, Arg a1s) (c2s, Arg a2s)
    | c1s == c2s && a1s /= a2s = False
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
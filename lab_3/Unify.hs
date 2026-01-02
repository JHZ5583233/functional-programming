module Unify (mgu, applyUnifier) where

import Types
import Data.Maybe (isNothing, fromJust)

mgu :: FuncApplication -> FuncApplication -> Maybe Unifier
mgu (FuncApp p1 as1) (FuncApp p2 as2)
    | p1 /= p2 = Nothing
    | length as1 /= length as2 = Nothing
    | any incorrectAssignment zs = Nothing
    | null rs = Just []
    | invalid rs = Nothing
    | otherwise = Just (correctType ras)
        where
            zs = zip as1 as2
            rs = reduceConstConst zs
            ds = reduceDuplicate rs
            us = unify [] ds
            ras = replace [] us

invalid :: [(Argument, Argument)] -> Bool
invalid [] = False
invalid (a@(Arg _, Const _):as) = invalidHelp as a || invalid as
invalid (_:as) = invalid as

invalidHelp :: [(Argument, Argument)] -> (Argument, Argument) -> Bool
invalidHelp [] _ = False
invalidHelp ((Arg a1, Const a2):as) ar@(Arg ar1, Const ar2)
    | a1 == ar1 && a2 /= ar2 = True
    | otherwise = invalidHelp as ar
invalidHelp (_:as) ar = invalidHelp as ar

isElem :: [(Argument, Argument)] -> (Argument, Argument) -> Bool
isElem [] _ = False
isElem ((a1, a2):as) ar@(ar1, ar2)
    | argName a1 == argName ar1 && argName a2 == argName ar2 = True
    | otherwise = isElem as ar

reduceDuplicate :: [(Argument, Argument)] -> [(Argument, Argument)]
reduceDuplicate [] = []
reduceDuplicate (a:as)
    | d = reduceDuplicate as
    | otherwise = a : reduceDuplicate as
        where
            d = isElem as a

argName :: Argument -> String
argName (Arg s)   = s
argName (Const s) = s

incorrectAssignment :: (Argument, Argument) -> Bool
incorrectAssignment (Const a1, Const a2) = a1 /= a2
incorrectAssignment _ = False

reduceConstConst :: [(Argument, Argument)] -> [(Argument, Argument)]
reduceConstConst [] = []
reduceConstConst ((Const _, Const _):as) = reduceConstConst as
reduceConstConst (a@(Arg a1, Arg a2):as)
    |a1 == a2 = reduceConstConst as
    | otherwise = a : reduceConstConst as
reduceConstConst (a:as) = a : reduceConstConst as

correctType :: [(Argument, Argument)] -> [(String, Argument)]
correctType [] = []
correctType ((c@(Const _), a):rs) = (argName a, c) : correctType rs
correctType ((a1, a2):as) = (argName a1, a2) : correctType as

canMerge :: [(Argument, Argument)] -> (Argument, Argument) -> Bool
canMerge [] _ = False
canMerge ((a1, a2):as) ar@(ar1, ar2)
    | argName a1 == argName ar1 = True
    | otherwise = canMerge as ar

unify :: [(Argument, Argument)] -> [(Argument, Argument)] -> [(Argument, Argument)]
unify rs [] = rs
unify rs (a:as)
    | isNothing ms = unify (a:rs) as
    | otherwise = unify [] (fromJust ms ++ as)
    where
        ms = indUnify rs a

indUnify :: [(Argument, Argument)] -> (Argument, Argument) -> Maybe [(Argument, Argument)]
indUnify as a
    | m = Just (indUnifyHelp as a)
    | otherwise = Nothing
    where
        m = canMerge as a

indUnifyHelp :: [(Argument, Argument)] -> (Argument, Argument) -> [(Argument, Argument)]
indUnifyHelp [] _ = []
indUnifyHelp (a@(a1, a2):as) ar@(ar1, ar2)
    | argName a1 == argName ar1 = (a1, ar2) : (a2, ar2) : indUnifyHelp as ar
    | otherwise = a : indUnifyHelp as ar

canReplace :: [(Argument, Argument)] -> (Argument, Argument) -> Bool
canReplace [] _ = False
canReplace ((a1, a2):as) ar@(ar1, ar2)
    | argName a2 == argName ar1 = True
    | otherwise = canMerge as ar

replace :: [(Argument, Argument)] -> [(Argument, Argument)] ->  [(Argument, Argument)]
replace rs [] = rs
replace rs (a:as)
    | r = replace [] (a:replaceHelp (rs ++ as) a)
    | otherwise = replace (a:rs) as
    where
        r = canReplace (rs ++ as) a

replaceHelp :: [(Argument, Argument)] -> (Argument, Argument) ->  [(Argument, Argument)]
replaceHelp [] _ = []
replaceHelp (a@(a1, a2):as) ar@(ar1, ar2)
    | argName a2 == argName ar1 = (a1, ar2) : replaceHelp as ar
    | otherwise = a:replaceHelp as ar

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
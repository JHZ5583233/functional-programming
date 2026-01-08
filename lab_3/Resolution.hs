module Resolution (resolveClauses, argIter, makeAllFacts) where

import Types
import Unify
import Data.Maybe (isNothing, catMaybes, isJust)

resolveClauses :: Clauses -> Clauses
resolveClauses cs = removeDuplicates (cs ++ resolveHelp rs fs)
    where
        rs = [x | x <- cs, length x > 1]
        fs = [x | x <- cs, length x == 1]

removeDuplicates :: Clauses -> Clauses
removeDuplicates [] = []
removeDuplicates (c:cs) = c : removeDuplicates (filter (not . clausesEqual c) cs)
    where
        clausesEqual a b = length a == length b && and (zipWith literalEqual a b)
        literalEqual (f1, p1) (f2, p2) = funcEqual f1 f2 && p1 == p2
        funcEqual (FuncApp n1 args1) (FuncApp n2 args2) = n1 == n2 && length args1 == length args2 && and (zipWith argEqual args1 args2)
        argEqual (Const s1) (Const s2) = s1 == s2
        argEqual (Arg s1) (Arg s2) = s1 == s2
        argEqual _ _ = False

resolveHelp :: Clauses -> Clauses -> Clauses
resolveHelp _ [] = []
resolveHelp rs (f:fs) = cs ++ resolveHelp rs fs
    where
        cs = applyAllFact rs f

applyAllFact :: Clauses -> Clause -> Clauses
applyAllFact rs f = concat [applyRules rs x | x <- fs]
    where
        fs = makeAllFacts f

applyRules :: Clauses -> Clause -> Clauses
applyRules [] _ = []
applyRules (r:rs) f
    | isNothing cs = applyRules rs f
    | otherwise = extractJust cs ++ applyRules rs f
    where
        cs = applyRule r f
        extractJust (Just x) = x
        extractJust Nothing = []

makeAllFacts :: Clause -> Clauses
makeAllFacts ((FuncApp ss as, b):cs) = [[(FuncApp ss x, b)] | x <- fcoms]
    where
        coms = (argIter as)
        fcoms = take (length coms - 1) coms

argName :: Argument -> String
argName (Const a) = a
argName (Arg a) = a

argIter :: [Argument] -> [[Argument]]
argIter [] = [[]]
argIter (a:as) = [x : y | x <- [a, Arg (argName a ++ "x")], y <- argIter as]

applyRule :: Clause -> Clause -> Maybe Clauses
applyRule r f
    | null cs = Nothing
    | otherwise = Just result
    where
        queryFunc = head (extractFunc f)
        ruleFuncs = extractFunc r
        unifiers = [mgu queryFunc x | x <- ruleFuncs]
        cs = [y | y <- unifiers, isJust y]
        nonMatchingPairs = [x | (x, u) <- zip r unifiers, isNothing u]
        unifiedSubst = concat (catMaybes unifiers)
        result = [[(applyUnifier unifiedSubst x, y) | (x, y) <- nonMatchingPairs]]

extractFunc :: Clause -> [FuncApplication]
extractFunc [] = []
extractFunc cs = [x | (x, _) <- cs]

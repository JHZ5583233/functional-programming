module Resolution (resolveClauses) where

import Types
import Unify

resolveClauses :: Clauses -> Clauses
resolveClauses cs = cs ++ resolveHelp rs fs
    where
        rs = [x | x <- cs, length x > 1]
        fs = [x | x <- cs, length x == 1]

resolveHelp :: Clauses -> Clauses -> Clauses
resolveHelp _ [] = []
resolveHelp rs (f:fs) = cs ++ resolveClauses rs fs
    where
        cs = applyRule rs f

applyRules :: Clauses -> Clause -> Clauses
applyRules [] _ = []
applyRules (r:rs) fs
    | isNothing cs = resolveClauses rs fs
    | otherwise = cs ++ resolveClauses rs fs
    where
        cs = applyRule r fs

applyRule :: Clause -> Clause -> Maybe Clauses
applyRule rs fs
    | null cs = Nothing
    | otherwise = rs
    where
        f = head (extractFunc fs)
        fus = extractFunc rs
        us = [mgu f x | x <- fus]
        cs = [y | y <- us, not (isNothing y)]
        ffus = [x | (x, u) <- zip rs us, isNothing u]
        uus = concat cs
        rs = [(applyUnifier uus x, y) | (x, y) <- ffus]

extractFunc :: Clause -> [FuncApplication]
extractFunc [] = []
extractFunc cs = [x | (x, _) <- cs]

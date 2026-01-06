module Resolution (resolveClauses) where

import Types

resolveClauses :: Clauses -> Clauses
resolveClauses cs = cs ++ resolveHelp rs fs
    where
        rs = [x | x <- cs, length x > 1]
        fs = [x | x <- cs, length x == 1]

resolveHelp :: Clauses -> Clauses -> Clauses
resolveHelp _ [] = []
resolveHelp rs (f:fs) = []

applyRule :: Clauses -> Clause -> Maybe Clauses
applyRule _ _ = Nothing

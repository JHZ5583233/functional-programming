module Resolution (resolveClauses) where

import Types

resolveClauses :: Clauses -> Clauses
resolveClauses cs = cs ++ resolveHelp rs fs
    where
        rs = [x | x <- cs, length x > 1]
        fs = [x | x <- cs, length x == 1]

resolveHelp :: [Clause] -> [Clause] -> [Clause]
resolveHelp _ [] = []
resolveHelp rs (f:fs) = []

applyRule :: [Clause] -> Clause -> Maybe [Clause]

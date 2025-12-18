module Clause (programToClauses) where

import Types
import Error

programToClauses :: Program -> Clauses
programToClauses (Program stmts) = concatMap stmtToClause stmts

stmtToClause :: (Statement, Int) -> Clauses
stmtToClause (Fact f, _) =
    [[(f, True)]]

stmtToClause (Rule hp bps, _) =
    [ map (,False) bps ++ [(hp, True)] ]

stmtToClause (Query _, _) =
    []


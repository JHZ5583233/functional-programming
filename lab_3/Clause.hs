module Clause (programToClauses) where

import Types
import Error

programToClauses :: Program -> Clauses
programToClauses [] = []
programToClauses (s:ps) =
    case s of
        Query _ ->
            programToClauses ps

        Fact f ->
            [(f, True)] : programToClauses ps

        Rule head body ->
            (map (, False) body ++ [(head, True)]) : programToClauses ps

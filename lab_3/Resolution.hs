module Resolution (resolveClauses) where

import Types
import Unify
import Data.Maybe (isJust, fromMaybe)

resolveClauses :: Clauses -> Clauses
resolveClauses = fixpoint
  where
    fixpoint clauses =
      let rs = filter ((> 1) . length) clauses
          newClauses = removeDuplicates $ map normalizeClause (clauses ++ resolveHelp rs clauses)
      in if length newClauses == length clauses
         then newClauses
         else fixpoint newClauses

literalEqual :: (FuncApplication, Bool) -> (FuncApplication, Bool) -> Bool
literalEqual (f1, p1) (f2, p2) = p1 == p2 && funcEqual f1 f2

funcEqual :: FuncApplication -> FuncApplication -> Bool
funcEqual (FuncApp n1 args1) (FuncApp n2 args2) =
    n1 == n2 && length args1 == length args2 && and (zipWith argEqual args1 args2)

argEqual :: Argument -> Argument -> Bool
argEqual (Const s1) (Const s2) = s1 == s2
argEqual (Arg s1) (Arg s2) = s1 == s2
argEqual _ _ = False

normalizeClause :: Clause -> Clause
normalizeClause [] = []
normalizeClause (l:ls) = l : normalizeClause (filter (not . literalEqual l) ls)

clausesEqual :: Clause -> Clause -> Bool
clausesEqual a b = length a == length b && and (zipWith literalEqual a b)

removeDuplicates :: Clauses -> Clauses
removeDuplicates [] = []
removeDuplicates (c:cs) = c : removeDuplicates (filter (not . clausesEqual c) cs)

resolveHelp :: Clauses -> Clauses -> Clauses
resolveHelp rs = concatMap (applyRules rs)

applyRules :: Clauses -> Clause -> Clauses
applyRules rs f = concatMap (\r -> maybe [] id (applyRule r f)) rs

applyRule :: Clause -> Clause -> Maybe Clauses
applyRule r f@((fFunc, fPol):_)
    | null matches = Nothing
    | otherwise = Just [resolveAt idx unifier | (idx, unifier) <- matches]
    where
        matches = [(i, u) | (i, (rFunc, rPol)) <- zip [0..] r,
                            rPol /= fPol,
                            let u = mgu fFunc rFunc,
                            isJust u]

        resolveAt matchIdx unifier =
            let substs = fromMaybe [] unifier
                remainingFromF = [(applyUnifier substs func, pol) | (func, pol) <- tail f]
                remainingFromR = [(applyUnifier substs func, pol) | (i, (func, pol)) <- zip [0..] r, i /= matchIdx]
            in remainingFromF ++ remainingFromR
applyRule _ [] = Nothing

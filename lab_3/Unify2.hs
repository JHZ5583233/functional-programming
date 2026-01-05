module Unify2 (mgu, applyUnifier) where

import Types
import Data.Maybe (isNothing, fromJust)
import Debug.Trace (trace)

-- Main unification function
mgu :: FuncApplication -> FuncApplication -> Maybe Unifier
mgu f1 f2 = trace ("mgu called with: " ++ show f1 ++ ", " ++ show f2) $
  if p1 /= p2 then Nothing
  else if length as1 /= length as2 then Nothing
  else if any incorrectAssignment zs then Nothing
  else if null rs then Just []
  else if invalid rs then Nothing
  else Just (correctType ras)
  where
    FuncApp p1 as1 = f1
    FuncApp p2 as2 = f2
    zs = zip as1 as2
    rs = reduceConstConst zs
    ds = reduceDuplicate rs
    us = unify [] ds
    ras = replace [] us

-- Invalid constant-variable combinations
invalid :: [(Argument, Argument)] -> Bool
invalid xs = trace ("invalid called with: " ++ show xs) $
  case xs of
    [] -> False
    (a@(Arg _, Const _):as) -> invalidHelp as a || invalid as
    (_:as) -> invalid as

invalidHelp :: [(Argument, Argument)] -> (Argument, Argument) -> Bool
invalidHelp xs ar = trace ("invalidHelp called with: " ++ show (xs, ar)) $
  case xs of
    [] -> False
    ((Arg a1, Const a2):as) -> let (Arg ar1, Const ar2) = ar
                               in if a1 == ar1 && a2 /= ar2 then True
                                  else invalidHelp as ar
    (_:as) -> invalidHelp as ar

isElem :: [(Argument, Argument)] -> (Argument, Argument) -> Bool
isElem xs ar = trace ("isElem called with: " ++ show (xs, ar)) $
  case xs of
    [] -> False
    ((a1, a2):as) -> let (ar1, ar2) = ar
                     in if argName a1 == argName ar1 && argName a2 == argName ar2 then True
                        else isElem as ar

reduceDuplicate :: [(Argument, Argument)] -> [(Argument, Argument)]
reduceDuplicate xs = trace ("reduceDuplicate called with: " ++ show xs) $
  case xs of
    [] -> []
    (a:as) -> if isElem as a then reduceDuplicate as else a : reduceDuplicate as

argName :: Argument -> String
argName x = trace ("argName called with: " ++ show x) $
  case x of
    Arg s -> s
    Const s -> s

incorrectAssignment :: (Argument, Argument) -> Bool
incorrectAssignment x = trace ("incorrectAssignment called with: " ++ show x) $
  case x of
    (Const a1, Const a2) -> a1 /= a2
    _ -> False

reduceConstConst :: [(Argument, Argument)] -> [(Argument, Argument)]
reduceConstConst xs = trace ("reduceConstConst called with: " ++ show xs) $
  case xs of
    [] -> []
    ((Const _, Const _):as) -> reduceConstConst as
    (a@(Arg a1, Arg a2):as) -> if a1 == a2 then reduceConstConst as else a : reduceConstConst as
    (a:as) -> a : reduceConstConst as

correctType :: [(Argument, Argument)] -> [(String, Argument)]
correctType xs = trace ("correctType called with: " ++ show xs) $
  case xs of
    [] -> []
    ((c@(Const _), a):rs) -> (argName a, c) : correctType rs
    ((a1, a2):as) -> (argName a1, a2) : correctType as

canMerge :: [(Argument, Argument)] -> (Argument, Argument) -> Bool
canMerge xs ar = trace ("canMerge called with: " ++ show (xs, ar)) $
  case xs of
    [] -> False
    ((a1, _):as) -> let (ar1, _) = ar in argName a1 == argName ar1 || canMerge as ar

unify :: [(Argument, Argument)] -> [(Argument, Argument)] -> [(Argument, Argument)]
unify rs xs = trace ("unify called with: " ++ show (rs, xs)) $
  case xs of
    [] -> rs
    (a:as) -> let ms = indUnify rs a
              in if isNothing ms then unify (a:rs) as
                 else unify [] (fromJust ms ++ as)

indUnify :: [(Argument, Argument)] -> (Argument, Argument) -> Maybe [(Argument, Argument)]
indUnify as a = trace ("indUnify called with: " ++ show (as, a)) $
  if canMerge as a then Just (indUnifyHelp as a) else Nothing

indUnifyHelp :: [(Argument, Argument)] -> (Argument, Argument) -> [(Argument, Argument)]
indUnifyHelp xs ar = trace ("indUnifyHelp called with: " ++ show (xs, ar)) $
  case xs of
    [] -> []
    (a@(a1, a2):as) -> let (ar1, ar2) = ar
                        in if argName a1 == argName ar1 then (a1, ar2):(a2, ar2):indUnifyHelp as ar
                           else a : indUnifyHelp as ar

canReplace :: [(Argument, Argument)] -> (Argument, Argument) -> Bool
canReplace xs ar = trace ("canReplace called with: " ++ show (xs, ar)) $
  case xs of
    [] -> False
    ((_, a2):as) -> let (ar1, _) = ar in argName a2 == argName ar1 || canReplace as ar

replaceHelp :: [(Argument, Argument)] -> (Argument, Argument) -> [(Argument, Argument)]
replaceHelp xs ar = trace ("replaceHelp called with: " ++ show (xs, ar)) $
  case xs of
    [] -> []
    (a@(a1, a2):as) -> let (ar1, ar2) = ar
                        in if argName a2 == argName ar1 then (a1, ar2) : replaceHelp as ar
                           else a : replaceHelp as ar

replace :: [(Argument, Argument)] -> [(Argument, Argument)] -> [(Argument, Argument)]
replace rs xs = trace ("replace called with: " ++ show (rs, xs)) $
  case xs of
    [] -> rs
    (a:as) -> if canReplace (rs ++ as) a then replace [] (a : replaceHelp (rs ++ as) a)
              else replace (a:rs) as

applyUnifier :: Unifier -> FuncApplication -> FuncApplication
applyUnifier us f@(FuncApp cs as) =
  trace ("applyUnifier called with: " ++ show (us, f)) $
    FuncApp cs (subArgs us as)

subArgs :: Unifier -> [Argument] -> [Argument]
subArgs us xs = trace ("subArgs called with: " ++ show (us, xs)) $
  case xs of
    [] -> []
    (Const cs:as) -> Const cs : subArgs us as
    (Arg cs:as) -> getConst cs us : subArgs us as

getConst :: String -> Unifier -> Argument
getConst ss us = trace ("getConst called with: " ++ show (ss, us)) $
  case us of
    [] -> Arg ss
    ((cs, a):us') -> if ss == cs then a else getConst ss us'

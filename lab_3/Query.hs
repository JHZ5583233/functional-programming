module Query (answerQueries) where

import Types
import Resolution
import Clause
import Types (Argument)
import Data.String (String)
import Data.List (sort)

answerQueries :: Program -> String
answerQueries ps = answerQueriesHelp qs cs
    where
        qs = getQueries ps
        cs = concat (resolveClauses (programToClauses ps))

getQueries :: Program -> [FuncApplication]
getQueries (Program []) = []
getQueries (Program ((Query qry, ln):rs)) = qry : getQueries (Program rs)
getQueries (Program (_:rs)) = getQueries (Program rs)

hasArguments :: FuncApplication -> Bool
hasArguments (FuncApp _ as) = d as
    where
        d :: [Argument] -> Bool
        d [] = False
        d (Arg _ : as) = True
        d (_:as) = d as

answerQueriesHelp :: [FuncApplication] -> Clause -> String
answerQueriesHelp [] _ = ""
answerQueriesHelp (f:fs) cs
    | hasArguments f = answerQueriesArg f cs ++ answerQueriesHelp fs cs
    | otherwise = answerQueriesConst f cs ++ answerQueriesHelp fs cs

answerQueriesConst :: FuncApplication -> Clause -> String
answerQueriesConst f cs
    | isElementClause f cs = show f ++ ": yes\n"
    | otherwise = show f ++ ": no\n"

isElementClause :: FuncApplication -> Clause -> Bool
isElementClause _ [] = False
isElementClause f ((c, _):cs) = isSameFunc f c || isElementClause f cs

isSameFunc :: FuncApplication -> FuncApplication -> Bool
isSameFunc (FuncApp name1 args1) (FuncApp name2 args2) = name1 == name2 && argsEqual args1 args2

argsEqual :: [Argument] -> [Argument] -> Bool
argsEqual [] [] = True
argsEqual (Const a:as) (Const b:bs) = a == b && argsEqual as bs
argsEqual (Arg a:as) (Arg b:bs) = a == b && argsEqual as bs
argsEqual _ _ = False

answerQueriesArg :: FuncApplication -> Clause -> String
answerQueriesArg f@(FuncApp fname args) cs = show f ++ ": " ++ formatvars vars  ++ " <- [" ++ ms ++ "]"
    where
        vars = concatMap (\(Arg v) -> v) (filter isArg args)
        isArg (Arg _) = True
        isArg _ = False
        numVars = length (filter isArg args)
        ms = concat (insertCommas (format numVars (makeString (extractMatches f cs))))

extractMatches :: FuncApplication -> Clause -> [[Argument]]
extractMatches _ [] = []
extractMatches fa@(FuncApp f args) ((FuncApp fc cArgs, _):cs)
    | f /= fc = extractMatches fa cs
    | otherwise = extractArguments args cArgs : extractMatches fa cs

extractArguments :: [Argument] -> [Argument] -> [Argument]
extractArguments _ [] = []
extractArguments [] _ = []
extractArguments (Arg _:as) (arg@(Const _):cArgs) = arg : extractArguments as cArgs
extractArguments (Arg _:as) (arg@(Arg _):cArgs) = extractArguments as cArgs
extractArguments (Const _:as) (_:cArgs) = extractArguments as cArgs

insertComma :: String -> String
insertComma [] = []
insertComma [c] = [c]
insertComma (c : cs@(cc :ccs)) = c : ',' : insertComma cs

insertCommas :: [String] -> [String]
insertCommas [] = []
insertCommas [c] = [c]
insertCommas (c : cs) = c : "," : insertCommas cs

makeString :: [[Argument]] -> [[String]]
makeString [] = []
makeString (as:ass) = ss : makeString ass
    where
        eArg (Arg a) = a
        eArg (Const a) = a

        ss = map eArg as

format :: Int -> [[String]] -> [String]
format _ [] = []
format numVars xss = sort (nubOrd (format' xss))
  where
    format' [] = []
    format' (ss:sss)
        | null ss = format' sss
        | length ss /= numVars = format' sss
        | length ss == 1 = head ss : format' sss
        | otherwise = ("(" ++ concat (insertCommas ss) ++ ")") : format' sss

nubOrd :: Eq a => [a] -> [a]
nubOrd [] = []
nubOrd (x:xs) = x : nubOrd (filter (/= x) xs)

formatvars :: String -> String
formatvars ss
    | length ss == 1 = ss
    | otherwise = "(" ++ insertComma (sort ss) ++ ")"
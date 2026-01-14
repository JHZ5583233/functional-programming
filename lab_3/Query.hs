module Query (answerQueries) where

import Types
import Resolution
import Clause

answerQueries :: Program -> String
answerQueries ps = answerQueriesHelp qs cs
    where
        qs = getQueries ps
        cs = concat (resolveClauses (programToClauses ps))

getQueries :: Program -> [FuncApplication]
getQueries (Program []) = []
getQueries (Program ((Query qry, ln):rs)) = qry : getQueries (Program rs)
getQueries (Program (_:rs)) = getQueries (Program rs)


answerQueriesHelp :: [FuncApplication] -> Clause -> String
answerQueriesHelp [] _ = ""
answerQueriesHelp (f:fs) cs
    | isElementClause f cs = show f ++ ": yes\n" ++ answerQueriesHelp fs cs
    | otherwise = show f ++ ": no\n" ++ answerQueriesHelp fs cs

isElementClause :: FuncApplication -> Clause -> Bool
isElementClause _ [] = False
isElementClause f ((c, _):cs) = (isSameFunc f c) && isElementClause f cs

isSameFunc :: FuncApplication -> FuncApplication -> Bool
isSameFunc (FuncApp name1 args1) (FuncApp name2 args2) = name1 == name2 && argsEqual args1 args2

argsEqual :: [Argument] -> [Argument] -> Bool
argsEqual [] [] = True
argsEqual (Const a:as) (Const b:bs) = a == b && argsEqual as bs
argsEqual (Arg a:as) (Arg b:bs) = a == b && argsEqual as bs
argsEqual _ _ = False
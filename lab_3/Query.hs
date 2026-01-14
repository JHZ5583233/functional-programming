module Query (answerQueries) where

import Types
import Resolution
import Clause

answerQueries :: Program -> String
answerQueries ps = concatMap (`answerQueriesConst` cs) qs
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
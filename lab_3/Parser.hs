module Parser(parseProgram) where

import Types
import Lexer
import Error
import Distribution.Simple.Utils (xargs)
import Types (Statement(Query))

parseProgram :: String -> Program
parseProgram input = Program (parseProlog [] (lexer input))

parseProlog :: [(Statement,Int)] -> [(LexToken,Int)] -> [(Statement,Int)]
-- implement this yourself
-------------------------------------------------------------------------------
{- parser for the following gramar:
Prolog         -> Statement Prolog
Prolog         -> <empty>

Statement      -> '?-' Relation '.'
Statement      -> Relation Statement'
Statement'     -> ':-' RelationList '.'
Statement'     -> '.'

RelationList   -> Relation RelationList'
RelationList'  -> ',' Relation RelationList'
RelationList'  -> <empty>

Relation       -> Identifier Args

Args           -> '(' ArgList ')'

ArgList        -> Argument ArgList'
ArgList'       -> ',' Argument ArgList'
ArgList'       -> <empty>

Argument       -> <variable> | <constant>
-}

parseProlog ss [] = reverse ss
parseProlog ss ls = parseProlog (s:ss) rls
    where
        h = head ls
        rs = takeWhile (\x -> snd h == snd x) ls
        pss = h:rs
        s = parseStatement pss
        rls = dropWhile (\x -> snd h == snd x) ls

parseStatement :: [(LexToken,Int)] -> (Statement,Int)
parseStatement (x:xs)
    | QueryTok <- fst x = (parseQuery (takeWhile (\fs -> fst fs /= DotTok) xs), n)
    where
        n = snd x

parseQuery :: [(LexToken,Int)] -> Statement
parseQuery xs = Query (FuncApp r as)
    where
        (r, as) = parseRelation xs

parseRelation :: [(LexToken,Int)] -> (String, [Argument])
parseRelation (l:ls) = (show l, as)
    where
        as = argHelper ls

argHelper :: [(LexToken,Int)] -> [Argument]
argHelper [] = []
argHelper (l:ls)
    | t `elem` [LparTok, CommaTok] = argHelper ls
    | RparTok <- t = []
    | IdentTok _ <- t = Const (show t) : argHelper ls
    | VarTok _ <- t = Arg (show t) : argHelper ls
    where
        t = fst l
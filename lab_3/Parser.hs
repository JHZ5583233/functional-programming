module Parser(parseProgram) where

import Types
import Lexer
import Error
import Distribution.Simple.Utils (xargs)
import System.Posix.Internals (lstat)
import Error (expectedError)

parseProgram :: String -> Program
parseProgram input = Program (parseProlog [] (lexer input))

parseProlog :: [(Statement,Int)] -> [(LexToken,Int)] -> [(Statement,Int)]
parseProlog ss [] = reverse ss

parseProlog ss ls = parseProlog (statement lls:ss) rs
    where
        (_, n) = head ls
        lls = takeWhile (\x -> snd x == n) ls
        rs = drop (length lls) ls
-- implement this yourself
-------------------------------------------------------------------------------
{- parser for the following grammar:
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

argument :: (LexToken,Int) -> Argument
argument (VarTok name, _) = Const name
argument (IdentTok name, _) = Arg name
argument (_, _) = printError "Not an argument."

parseArgList :: [(LexToken,Int)] -> [Argument]
parseArgList [] = []
parseArgList ((CommaTok, _):xs) = parseArgList xs
parseArgList (x:xs) = argument x : parseArgList xs

args :: [(LexToken,Int)] -> ([Argument], [(LexToken,Int)])
args [] = ([], [])
args ((LparTok, n):xs)
    | null rs = eofError
    | null as = expectedError n "a literal"
    | otherwise = (parseArgList as, tail rs)
        where
            as = takeWhile (\x -> fst x /= RparTok) xs
            rs = dropWhile (\x -> fst x /= RparTok) xs
args ((_, n):xs) = expectedError n "an opening parenthesis"

relation :: [(LexToken,Int)] -> (FuncApplication, [(LexToken,Int)])
relation [] = eofError
relation ((IdentTok x,_):xs) = (FuncApp x as, r)
    where
        (as, r) = args xs
relation ((_, n):_) = expectedError n "a relation name (i.e. an identifier that start with a lower case letter)"

relationList' :: [(LexToken, Int)] -> ([FuncApplication], [(LexToken,Int)])
relationList' [] = ([], [])
relationList' ((CommaTok, _):xs) = relationList xs
relationList' i = printError (show i)

relationList :: [(LexToken, Int)] -> ([FuncApplication], [(LexToken,Int)])
relationList ls = (f : ns, rrs)
    where
        (f, rs) = relation ls
        (ns, rrs) = relationList' rs

statement' :: [(LexToken, Int)] -> [FuncApplication]
statement' [] = eofError
statement' (x:xs)
    | fst x == DotTok = []
    | null ds = expectedError (snd x) "a relation name (i.e. an identifier that start with a lower case letter)"
    | fst x == FollowsTok = fst (relationList ds)
    | otherwise = printError "Missing Dot"
    where
        ds = takeWhile (\x -> fst x /= DotTok) xs

statement :: [(LexToken, Int)] -> (Statement, Int)
statement ((QueryTok, i):xs) = (Query (fst (relation (takeWhile (\x -> fst x /= DotTok) xs))), i)
statement ls
    | null as = (Fact s, n)
    | otherwise = (Rule s as, n)
        where
            (_, n) = head ls
            (s, rs) = relation ls
            as = statement' rs

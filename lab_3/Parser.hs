module Parser(parseProgram) where

import Types
import Lexer
import Error
import Distribution.Simple.Utils (xargs)

parseProgram :: String -> Program
parseProgram input = Program (parseProlog [] (lexer input))

parseProlog :: [(Statement,Int)] -> [(LexToken,Int)] -> [(Statement,Int)]
parseProlog [] [] = []
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
args ((LparTok, _):xs) = (parseArgList (takeWhile (\x -> fst x /= RparTok) xs), r)
    where
        r = tail (dropWhile (\x -> fst x /= RparTok) xs)

relation :: [(LexToken, Int)] -> (FuncApplication, [(LexToken,Int)])
relation ((IdentTok x,_):xs) = (FuncApp x as, r)
    where
        (as, r) = args xs

relationList' :: [(LexToken, Int)] -> ([FuncApplication], [(LexToken,Int)])
relationList' [] = ([], [])
relationList' ((CommaTok, _):xs) = relationList xs
relationList' i = printError (show i)

relationList :: [(LexToken, Int)] -> ([FuncApplication], [(LexToken,Int)])
relationList ls = (f : ns, rrs)
    where
        (f, rs) = relation ls
        (ns, rrs) = relationList' rs

Statement'
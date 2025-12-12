module Lexer(LexToken(..),lexer) where
import Data.Char
import Error
import Distribution.Compat.Prelude (Integer, fromIntegral)
import Error (lexError)
import Data.Char (isAlphaNum)
import Data.Int (Int)

data LexToken = DotTok   | CommaTok | FollowsTok
              | QueryTok | LparTok  | RparTok
              | IdentTok String
              | VarTok String
  deriving Eq

instance Show LexToken where
  show DotTok            = "."
  show CommaTok          = ","
  show FollowsTok        = ":-"
  show QueryTok          = "?-"
  show LparTok           = "("
  show RparTok           = ")"
  show (IdentTok name)   = "<id:" ++ name ++ ">"
  show (VarTok name)     = "<var:" ++ name ++ ">"

lexer :: String -> [(LexToken,Int)]    -- The Int is the line number in the source code
-- Please implement this function yourself
lexer = lexHelper 1

lexHelper:: Int -> String -> [(LexToken, Int)]
lexHelper _ [] = []
lexHelper n str@(c:cs)
  | c `elem` "\t "  = lexHelper n cs -- skip whitespace
  | c == '\n' = lexHelper (n + 1) cs
  | c == '%' = lexHelper n (dropWhile (/= '\n') cs)
  | c == '.' = (DotTok, n) : lexHelper n cs
  | c == ',' = (CommaTok, n) : lexHelper n cs
  | c == ':' = check c n cs
  | c == '?' = check c n cs
  | c == '(' = (LparTok, n) : lexHelper n cs
  | c == ')' = (RparTok, n) : lexHelper n cs
  | isAlpha c && isUpper c = (VarTok (c : takeWhile isAlphaNum cs), n) : lexHelper n (dropWhile isAlphaNum cs)
  | isAlpha c && isLower c = (IdentTok (c : takeWhile isAlphaNum cs), n) : lexHelper n (dropWhile isAlphaNum cs)
  | otherwise = lexError n c

check :: Char -> Int -> String -> [(LexToken, Int)]
check c n cs
  | head cs /= '-' = lexError n c
  | c == ':' = (FollowsTok, n) : lexHelper n (tail cs)
  | c == '?' = (QueryTok, n) : lexHelper n (tail cs)
module Lexer(LexToken(..),lexer) where
import Data.Char
import Error

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

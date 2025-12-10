import Data.Char
import Debug

{- parser for the grammar:
    E -> T E’
    E’ -> + T E’
    E’ -> - T E’
    E’ -> <empty string>
    T -> F T’
    T’ -> * F T’
    T’ -> / F T’
    T’ -> % F T’
    T’ -> <empty string>
    F -> - F
    F -> ( E )
    F -> <integer>
-}

type Token  = String
type Tokens = [String]

lexer :: String -> Tokens
lexer [] = []
lexer str@(c:cs)
  | c `elem` "\n\t "  = lexer cs -- skip whitespace
  | c `elem` "*/+-%() " = [c]:lexer cs
  | isDigit c = takeWhile isDigit str : lexer (dropWhile isDigit str)
  | otherwise = abort $ printf "illegal character '%c' found." c

parseE :: Tokens -> (Integer, Tokens)
parseE toks = parseE' v toks'
  where
    (v, toks') = parseT toks

parseE' :: Integer -> Tokens -> (Integer, Tokens)
parseE' acc ("+":toks) =
  let (v, toks') = parseT toks
  in parseE' (acc + v) toks'
parseE' acc ("-":toks) =
  let (v, toks') = parseT toks
  in parseE' (acc - v) toks'
parseE' acc toks = (acc, toks)

parseT :: Tokens -> (Integer, Tokens)
parseT toks = parseT' v toks'
  where
    (v, toks') = parseF toks

parseT' :: Integer -> Tokens -> (Integer, Tokens)
parseT' acc ("*":toks) =
  let (v, toks') = parseF toks
  in parseT' (acc * v) toks'
parseT' acc ("/":toks) =
  let (v, toks') = parseF toks
  in parseT' (acc `div` v) toks'
parseT' acc ("%":toks) =
  let (v, toks') = parseF toks
  in parseT' (acc `mod` v) toks'
parseT' acc toks = (acc, toks)

parseF :: Tokens -> (Integer, Tokens)
parseF ("-":toks) =
  let (v, toks') = parseF toks
  in (-v, toks')

parseF [] = abort "error: unexpected end of input."
parseF ("(":toks) =
  let (v, toks') = parseE toks
  in case toks' of
        (")":rest) -> (v, rest)
        _ -> abort "error: expected ')'"

parseF (tok:toks)
  | all isDigit tok = (read tok, toks)
  | otherwise = abort $ printf "Error, unexpected '%s'." tok

evalParser :: String -> Integer
evalParser str = case parseE (lexer str) of
  (val, []) -> val
  (val, _) -> val

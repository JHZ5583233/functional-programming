module Error where
import System.Exit
import System.IO.Unsafe

abortWithMessage :: String -> IO a
abortWithMessage msg = do
  putStrLn msg
  exitSuccess

printError :: String -> a
printError msg = unsafePerformIO (abortWithMessage msg)
  
lexError :: Int -> Char -> a
lexError linenr ch = printError errmsg
  where errmsg = "Lexical error in line " ++ show linenr ++ ": unexpected character '" ++ [ch] ++ "'."

expectedError :: Int -> String -> a
expectedError linenr expmsg = printError errmsg
  where errmsg = show linenr ++ ":Syntax error, expected " ++ expmsg ++ "."

eofError :: a
eofError = printError "Syntax error: Unexpected end of file."

echo :: a -> String -> a
echo _ msg = printError msg

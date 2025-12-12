import System.Environment
import System.IO
import Lexer

main = do
  args <- getArgs
  let reader = if args == [] then getContents else readFile (head args)
  text <- reader
  print (lexer text)

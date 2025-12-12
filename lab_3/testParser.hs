import System.Environment
import System.IO
import Types
import Parser

process :: String -> Program
process str = parseProgram str

main = do
  args <- getArgs
  let reader = if args == [] then getContents else readFile (head args)
  text <- reader
  print (process text)

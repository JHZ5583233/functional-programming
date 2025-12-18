import System.Environment
import System.IO
import Types
import Parser

process :: String -> Program
process = parseProgram

main = do
  args <- getArgs
  let reader = if null args then getContents else readFile (head args)
  text <- reader
  print (process text)

import System.Environment
import System.IO
import Types
import Parser
import Analysis
import Clause

process :: String -> Program
process = programToClauses.analyse.parseProgram

main = do
  args <- getArgs
  let reader = if args == [] then getContents else readFile (head args)
  text <- reader
  print (process text)

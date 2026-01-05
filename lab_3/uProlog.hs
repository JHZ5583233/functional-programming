import System.Environment
import System.IO
import Types
import Parser
import Analysis
import Clause
import Resolution

process :: String -> Clauses
process = resolveClauses.programToClauses.analyse.parseProgram

main = do
  args <- getArgs
  let reader = if null args then getContents else readFile (head args)
  text <- reader
  print (process text)

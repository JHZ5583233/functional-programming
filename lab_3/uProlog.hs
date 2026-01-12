import System.Environment
import Parser
import Analysis
import Query

process :: String -> String
process = answerQueries.analyse.parseProgram

main = do
  args <- getArgs
  let reader = if null args then getContents else readFile (head args)
  text <- reader
  putStrLn (process text)
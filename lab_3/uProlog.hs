import System.Environment
import System.IO
import Types
import Parser
import Analysis
import Clause
import Resolution

process :: String -> Clauses
process = resolveClauses.programToClauses.analyse.parseProgram

-- Pretty print a single literal (FuncApplication, Bool)
prettyLiteral :: (FuncApplication, Bool) -> String
prettyLiteral (func, True) = show func
prettyLiteral (func, False) = "¬" ++ show func

-- Pretty print a clause (disjunction of literals)
prettyClause :: Clause -> String
prettyClause [] = "□"  -- Empty clause (contradiction)
prettyClause [lit] = prettyLiteral lit
prettyClause lits = unwords [prettyLiteral lit ++ (if i < length lits - 1 then " ∨" else "") | (lit, i) <- zip lits [0..]]

-- Pretty print all clauses
prettyClauses :: Clauses -> String
prettyClauses cs = unlines $ zipWith (\i c -> show i ++ ". " ++ prettyClause c) [1..] cs

main = do
  args <- getArgs
  let reader = if null args then getContents else readFile (head args)
  text <- reader
  putStrLn "\n=== Results ==="
  putStrLn (prettyClauses (process text))
  putStrLn ("Total clauses: " ++ show (length (process text)))

import System.Environment (getArgs)
import Types
import Parser
import Resolution
import Clause

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filename] -> do
            content <- readFile filename
            let prog = parseProgram content
            let clauses = programToClauses prog
            let resolvedClauses = resolveClauses clauses
            putStrLn "Original Clauses:"
            mapM_ print clauses
            putStrLn "\nResolved Clauses (list of lists):"
            mapM_ print resolvedClauses
            putStrLn "\nFlattened Resolved Clauses:"
            let flatResolved = concat resolvedClauses
            mapM_ print flatResolved
        _ -> putStrLn "Usage: testResolutionDebug <filename.upl>"

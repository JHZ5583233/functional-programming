module Main where

import Types
import Resolution (argIter, makeAllFacts)

-- Test helper function for argIter
testArgIter :: String -> [Argument] -> IO ()
testArgIter description input = do
    putStrLn $ "\nTest: " ++ description
    putStrLn $ "Input: " ++ show input
    putStrLn $ "Output:"
    let result = argIter input
    mapM_ (putStrLn . ("  " ++) . show) result
    putStrLn $ "Total combinations: " ++ show (length result)

-- Test helper function for makeAllFacts
testMakeAllFacts :: String -> Clause -> IO ()
testMakeAllFacts description clause = do
    putStrLn $ "\nTest: " ++ description
    putStrLn $ "Input clause: " ++ show clause
    putStrLn $ "Generated facts:"
    let result = makeAllFacts clause
    mapM_ (putStrLn . ("  " ++) . show) result
    putStrLn $ "Total facts generated: " ++ show (length result)

main :: IO ()
main = do
    putStrLn "Testing argIter and makeAllFacts functions"
    putStrLn "=========================================="
    
    putStrLn "\n--- argIter Tests ---"
    
    -- Test 1: Empty list
    testArgIter "Empty list" []
    
    -- Test 2: Single Const argument
    testArgIter "Single Const" [Const "x"]
    
    -- Test 3: Single Arg (variable)
    testArgIter "Single Arg" [Arg "X"]
    
    -- Test 4: Two Const arguments
    testArgIter "Two Const arguments" [Const "a", Const "b"]
    
    -- Test 5: Two Arg (variables)
    testArgIter "Two Arg variables" [Arg "X", Arg "Y"]
    
    -- Test 6: Mixed Const and Arg
    testArgIter "Mixed: Const and Arg" [Const "a", Arg "X"]
    
    -- Test 7: Three arguments
    testArgIter "Three arguments" [Arg "X", Arg "Y", Arg "Z"]
    
    -- Test 8: Longer list
    testArgIter "Four arguments" [Const "a", Const "b", Arg "X", Arg "Y"]
    
    putStrLn "\n--- makeAllFacts Tests ---"
    
    -- Test 1: Single-argument fact
    testMakeAllFacts 
        "Single constant fact" 
        [(FuncApp "p" [Const "a"], True)]
    
    -- Test 2: Two-argument fact
    testMakeAllFacts 
        "Two-argument fact" 
        [(FuncApp "p" [Const "a", Const "b"], True)]
    
    -- Test 3: Single-argument variable
    testMakeAllFacts 
        "Single variable fact" 
        [(FuncApp "p" [Arg "X"], True)]
    
    -- Test 4: Two-argument with mixed types
    testMakeAllFacts 
        "Mixed constant and variable" 
        [(FuncApp "bigger" [Const "elephant", Const "horse"], True)]
    
    -- Test 5: Clause with two literals
    testMakeAllFacts 
        "Two-literal clause" 
        [(FuncApp "bigger" [Const "a", Const "b"], False), 
         (FuncApp "isBigger" [Const "a", Const "b"], True)]
    
    -- Test 6: Three-argument fact
    testMakeAllFacts 
        "Three-argument fact" 
        [(FuncApp "compare" [Const "x", Const "y", Const "z"], True)]
    
    putStrLn "\n=========================================="
    putStrLn "argIter behavior:"
    putStrLn "  For each argument, generates combinations where:"
    putStrLn "  - The argument stays as-is, OR"
    putStrLn "  - It's replaced with modified argument name (e.g., X -> Xx)"
    putStrLn "  For n arguments, this produces 2^n combinations"
    putStrLn "\nmakeAllFacts behavior:"
    putStrLn "  Takes a clause and generates all facts by:"
    putStrLn "  - Using argIter to generate argument combinations"
    putStrLn "  - Dropping the first combination (original)"
    putStrLn "  - Creating new facts with each remaining combination"

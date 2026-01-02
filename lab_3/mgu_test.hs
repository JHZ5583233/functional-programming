module Main where

import Unify
import Types

-- Pretty print helper
printTest :: String -> FuncApplication -> FuncApplication -> IO ()
printTest name a b = do
    putStrLn ("Test: " ++ name)
    putStrLn ("  " ++ show a)
    putStrLn ("  " ++ show b)
    putStrLn ("  mgu = " ++ show (mgu a b))
    putStrLn ""

main :: IO ()
main = do
    let t1a = FuncApp "f" [Arg "A", Arg "C", Arg "E", Arg "E", Arg "C"]
    let t1b = FuncApp "f" [Arg "B", Arg "D",  Arg "D", Const "f",  Arg "B"]
    printTest "simple variable binding" t1a t1b

    let t1a = FuncApp "f" [Arg "A", Arg "C", Arg "E", Arg "E", Arg "C", Arg "E"]
    let t1b = FuncApp "f" [Arg "B", Arg "D",  Arg "D", Const "f",  Arg "B", Const "g"]
    printTest "simple variable binding" t1a t1b

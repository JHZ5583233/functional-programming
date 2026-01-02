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
    let t1a = FuncApp "f" [Const "a"]
    let t1b = FuncApp "f" [Const "a"]
    printTest "simple variable binding" t1a t1b

    let t1a = FuncApp "f" [Const "a"]
    let t1b = FuncApp "f" [Const "b"]
    printTest "simple variable binding" t1a t1b

    let t1a = FuncApp "f" [Const "a"]
    let t1b = FuncApp "g" [Const "a"]
    printTest "simple variable binding" t1a t1b

    let t1a = FuncApp "f" [Arg "X", Arg "Y"]
    let t1b = FuncApp "f" [Arg "A", Arg "A"]
    printTest "simple variable binding" t1a t1b

    let t1a = FuncApp "f" [Arg "A", Arg "A"]
    let t1b = FuncApp "f" [Arg "X", Arg "Y"]
    printTest "simple variable binding" t1a t1b

    let t1a = FuncApp "f" [Arg "A", Arg "A"]
    let t1b = FuncApp "f" [Arg "X", Arg "y"]
    printTest "simple variable binding" t1a t1b

    let t1a = FuncApp "f" [Arg "A", Arg "C", Arg "E", Arg "E", Arg "C"]
    let t1b = FuncApp "f" [Arg "B", Arg "D",  Arg "D", Const "f",  Arg "B"]
    printTest "simple variable binding" t1a t1b

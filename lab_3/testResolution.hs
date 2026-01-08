import Types
import Unify
import Resolution

-- Test data: Simple facts and rules
-- Fact: parent(alice, bob)
parentAliceBob :: Clause
parentAliceBob = [(FuncApp "parent" [Const "alice", Const "bob"], False)]

-- Fact: parent(bob, charlie)
parentBobCharlie :: Clause
parentBobCharlie = [(FuncApp "parent" [Const "bob", Const "charlie"], False)]

-- Rule: grandparent(X, Z) :- parent(X, Y), parent(Y, Z)
-- Represented as clause with multiple literals
grandparentRule :: Clause
grandparentRule = [
    (FuncApp "grandparent" [Arg "X", Arg "Z"], False),
    (FuncApp "parent" [Arg "X", Arg "Y"], True),
    (FuncApp "parent" [Arg "Y", Arg "Z"], True)
    ]

-- Fact: ancestor(X, X)
ancestorSelf :: Clause
ancestorSelf = [(FuncApp "ancestor" [Arg "X", Arg "X"], False)]

-- Test queries
query1 :: Clause
query1 = [(FuncApp "parent" [Const "alice", Const "bob"], True)]

query2 :: Clause
query2 = [(FuncApp "grandparent" [Const "alice", Const "charlie"], True)]

-- Test clauses set
testClauses1 :: Clauses
testClauses1 = [parentAliceBob, parentBobCharlie, query1]

testClauses2 :: Clauses
testClauses2 = [parentAliceBob, parentBobCharlie, grandparentRule, query2]

testClauses3 :: Clauses
testClauses3 = [ancestorSelf, query1]

-- Run tests
main :: IO ()
main = do
    putStrLn "Test 1: Simple fact query"
    print testClauses1
    putStrLn "\nResolution result:"
    print (resolveClauses testClauses1)
    
    putStrLn "\n\nTest 2: Rule and query"
    print testClauses2
    putStrLn "\nResolution result:"
    print (resolveClauses testClauses2)
    
    putStrLn "\n\nTest 3: Self-referencing rule"
    print testClauses3
    putStrLn "\nResolution result:"
    print (resolveClauses testClauses3)

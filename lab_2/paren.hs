balancedParens :: String -> Bool
balancedParens = startHelp

startHelp :: String -> Bool
startHelp s
    | null s = True
    | head s == '(' = endHelp (tail s)
    | otherwise = startHelp (tail s)

endHelp :: String -> Bool
endHelp s
    | null s = False
    | head s == ')' = start
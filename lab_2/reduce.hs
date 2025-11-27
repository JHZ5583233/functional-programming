removeRuns :: String -> String
removeRuns ss
    | length ss <= 1 = ss
    | head ss == head (tail ss) = removeRuns (tail ss)
    | otherwise = head ss : removeRuns (tail ss)
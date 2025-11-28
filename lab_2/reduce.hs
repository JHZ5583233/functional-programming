import Data.List (group)
removeRuns :: String -> String
removeRuns ss = map head (group ss)
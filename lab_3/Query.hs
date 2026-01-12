module Query (answerQueries) where

import Types

answerQueries :: Program -> String
answerQueries [] = ""
answerQueries ((Fact f, _):rs) = "/n" ++ answerQueries rs
answerQueries ((Query q, _):rs) = "/n" ++ answerQueries rs
answerQueries ((Rule ru rus, _):rs) = "/n" ++ answerQueries rs
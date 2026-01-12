module Query (answerQueries) where

import Types

answerQueries :: Program -> String
answerQueries (Program []) = ""
answerQueries (Program ((Fact f, _):rs)) = "/n" ++ answerQueries (Program rs)
answerQueries (Program ((Query q, _):rs)) = "/n" ++ answerQueries (Program rs)
answerQueries (Program ((Rule ru rus, _):rs)) = "/n" ++ answerQueries (Program rs)
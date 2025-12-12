module Analysis(analyse) where

import Data.List
import Types
import Error

numberOfArguments :: FuncApplication -> (String,Int)
numberOfArguments (FuncApp fun args) = (fun,length args)

arguments :: FuncApplication -> [String]
arguments (FuncApp fun args) = [ arg | Arg arg <- args]

reduce :: [(String,Int)] -> [(String,Int)]
reduce xs = sort [ head [ (id,linenr) | (ident,linenr) <- xs, ident==id] | id <- ids]
  where ids = nub [ str | (str,_) <- xs]

functions :: Program -> [(String,Int)]
functions (Program prog) = funcs prog []
  where
    funcs [] acc                          = (reduce.reverse) acc
    funcs ((Fact fa,linenr):prog) acc     = funcs prog (funcsFA linenr fa:acc)
    funcs ((Query fa,linenr):prog) acc    = funcs prog (funcsFA linenr fa:acc)
    funcs ((Rule fa fas,linenr):prog) acc = funcs prog (map (funcsFA linenr) (fa:fas) ++ acc)
    funcsFA linenr (FuncApp fun args)     = (fun,linenr)

functionDefs  :: Program -> [(String,Int,Int)]
-- first element is a pair consisting of function name + number of args, second element is line number
functionDefs (Program prog) = (reduce.sort) (funcdefs prog [])
  where
    funcdefs [] acc                        = acc
    funcdefs ((Fact fa,linenr):prog) acc   = (numberOfArguments fa,linenr):funcdefs prog acc
    funcdefs ((Rule fa _,linenr):prog) acc = (numberOfArguments fa,linenr):funcdefs prog acc
    funcdefs (_:prog) acc                  = funcdefs prog acc
    reduce []                              = []
    reduce (((fun,n),linenr):fdefs)        = (fun,n,linenr):reduce [ ((f,m),lnr) | ((f,m),lnr) <- fdefs, f/=fun || n/=m]

checkFunctionCall :: Int -> [(String,Int,Int)] -> FuncApplication -> Bool
checkFunctionCall linenr fdefs (FuncApp fun args) = passTest fdefs
  where
    passTest [] = False `echo` ("Semantic error: No definition of the relation '" ++ fun ++ "' in line " ++ show(linenr) ++ ".")
    passTest ((f,nargs,lnr):fdefs)
      | fun /= f              = passTest fdefs
      | nargs /= length args  = False `echo` ("Semantic error: Incorrect number of arguments in call of '" ++ fun ++ "' in line " ++ show(linenr) ++ ".")
      | otherwise             = True

checkFunctionCalls :: [(String,Int,Int)] -> Maybe Program -> Maybe Program
checkFunctionCalls _ Nothing = Nothing
checkFunctionCalls fdefs (Just (Program prog))
  | passTest prog = Just (Program prog)
  | otherwise     = Nothing
  where
    passTest [] = True
    passTest ((Fact fa,linenr):prog)          = (checkFunctionCall linenr fdefs fa) && passTest prog
    passTest ((Query fa,linenr):prog)         = (checkFunctionCall linenr fdefs fa) && passTest prog
    passTest ((Rule fa premises,linenr):prog) = (and (map (checkFunctionCall linenr fdefs) premises)) && (passTest prog)

checkNumberArgumentsDefs :: [(String,Int,Int)] -> Maybe Program -> Maybe Program
checkNumberArgumentsDefs _ Nothing = Nothing
checkNumberArgumentsDefs  fdefs prog
  | passTest fdefs = prog
  | otherwise      = Nothing
  where
    passTest ((f0,args0,lnr0):(f1,args1,lnr1):rest)
      | f0 /= f1  =  passTest ((f1,args1,lnr1):rest)
      | otherwise = False `echo` ("Semantic error: Number of arguments in definition of the relation '" ++
                                  f0 ++ "' differs in lines " ++ show(lnr0) ++ " and " ++ show(lnr1) ++ ".")
    passTest _ = True

checkFacts :: Maybe Program -> Maybe Program
checkFacts Nothing = Nothing
checkFacts (Just (Program prog))
  | passTest prog = Just (Program prog)
  | otherwise     = Nothing
  where
    passTest [] = True
    passTest ((Fact fa,linenr):prog)
      | arguments fa == []  = True
      | otherwise           = False `echo` ("Semantic error: Fact has an argument '" ++ (head (arguments fa)) ++
                                          "' in line " ++ show(linenr) ++ ".")
    passTest (_:prog)     = passTest prog

analyse :: Program -> Program
analyse prog = sure $ (checkFacts.(checkFunctionCalls fdefs).(checkNumberArgumentsDefs fdefs)) (Just prog)
  where
    fdefs = functionDefs prog
    sure (Just p) = p

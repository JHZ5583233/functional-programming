module Types where

data Argument = Const String
              | Arg String

data FuncApplication = FuncApp String [Argument]

data Statement = Fact FuncApplication
               | Rule FuncApplication [FuncApplication]
               | Query FuncApplication

data Program = Program [(Statement,Int)]

instance Show Argument where
  show (Const name) = name
  show (Arg name) = name

instance Show FuncApplication where
  show (FuncApp funcname args) = funcname ++ "(" ++ showLst args ++ ")"

instance Show Statement where
  show (Rule fa premises) = show fa ++ " :- " ++ showLst premises
  show (Fact fa) = show fa
  show (Query fa) = "?- " ++ show fa

instance Show Program where
  show (Program statements) = concat (map (\(st,line) -> show(line) ++ ":" ++ show(st) ++ "\n") statements)

-- show a list without enclosing [ ]
showLst :: Show a => [a] -> String
showLst = tail.init.show

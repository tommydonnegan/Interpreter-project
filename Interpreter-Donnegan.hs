--Thomas Donnegan
--Interpreter Project Questions 1-7
--Due 4/20/2021 11:59pm

type Variable = String
type Val = Int

--Problem #1 
--Command is a datatype that has 4 operations; 
--Operation #1 - Assign - Const requires an object of type Variable and an object of type Expr
--Operation #2 - Seq - Seq requires two objects of type Command
--Operation #3 - Cond - Cond requires an object of type Expr and two objects of type Command
--Operation #4 - While - While requires an object of type Expr and an object of type Command

data Command = Assign Variable Expr | Seq Command Command | Cond Expr Command Command | While Expr Command

--Problem #2
--Expr is a datatype that has 5 operations; 
--Operation #1 - Const - Const requires an object of type Val
--Operation #2 - Var - Var requires an object of type Variable
--Operation #3 - Minus - Minus requires two objects of type Expr
--Operation #4 - Times - Times requires two objects of type Expr
--Operation #5 - Greater - Greater requires two objects of type Expr

data Expr = Const Val | Var Variable | Minus Expr Expr | Times Expr Expr | Greater Expr Expr

type Store = Variable -> Val

--Problem #3
--Function name: eval
--Function type: Expr -> Store -> Val
eval :: Expr -> Store -> Val
eval (Const c1) _ = c1

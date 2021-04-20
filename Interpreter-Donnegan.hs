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
-- eval accepts an Expr and an object of type Store
-- Function name: eval 
-- Function type: Expr -> Store -> Val
-- when evaluating a (Const c) the store is ignored and eval(Const c) _ returns the Integer 'c' 
-- when evaluating a (Var v1) store if the store is store then eval(Var v1) store will determine the value of the v1 to the form Val
-- when evaluating a (Minus x y) s ,given a store s, the values of x and y are evaluated and ultimatley the Integer value of x-y is returned 
-- when evaluating a (Times x y) s ,given a store s, the values of x and y are evaluated and ultimatley the Integer value of x*y is returned
-- when evaluating a (Greater x y) s ,given a store s, the values of x and y are evaluated and ultimatley; if x is greater than y comapred when as integers then 1 is returned
--otherwise a zero is returned which is the same as being thought of False(0) and True(1)
eval :: Expr -> Store -> Val
eval (Const c) _ = c 
eval (Var v1) store = store v1
eval (Minus x y) s = eval x s - eval y s
eval (Times x y) s = eval x s * eval y s
eval (Greater x y) s = if eval x s > eval y s then 1  else 0

--Function name: switch
--Function type: Val -> (Store -> Store) -> (Store -> Store) -> Store -> Store
--Function purpose: The purpose of switch is to  apply the appropriate action to the store, where the choice is determined by its  first argument. 
--If this value is the integer 1, then switch’s first function argument is applied to the input store. If, on the other hand, this value is the integer 0,
--then its second function argument is applied to the input store.

switch :: Val -> (Store -> Store) -> (Store -> Store) -> Store -> Store
switch = undefined

--Function name: update
--Function type: Store -> Variable -> Val -> Store
--Function purpose: The purpose of update is to associate the value val with var, rather than associating with var whatever value s associates with it.
update :: Store -> Variable -> Val -> Store
update s var val = \queryVar -> if queryVar == var then val else s queryVar

--Function name: initial
--Function type: a -> Integer
--Function purpose: The purpose of initial is to return 0 whenever it is passed anything 
initial = \ _ -> 0

--Function name: fetch
--Function type: Store -> Variable -> Val
--Function purpose: Remembeering that store is defined as Variable -> Val we can simply return s as its the only thing that could type check
fetch :: Store -> Variable -> Val
fetch s = s

--Function name: interpret
--Function type: Command -> Store -> Store
--Function purpose: The purpose of the interpret function is to interpret L-Commands in the form of while loops relative to its inputted store.
interpret :: Command -> Store -> Store
--Problem #4
--When calling interpret(Assign v e) s its calls the update function and recieves as input the; given state 's', the given var 'v' and it evaluates the value of 'e' given state 's'
interpret (Assign v e) s = update s v (eval e s)
--Problem #5
--When calling interpret(Seq c1 c2) s it first creates a sotre named s1. The first command, 'c1' is interpreted with a store of 's'. Now this value s1 is inesterted into 
--interpret c2 s1. By doing this we ensure that both commands do not use the same store as data can be overwritten or modified depending on the function. 
interpret (Seq c1 c2) s = let s1 = interpret c1 s in interpret c2 s1
--Problem #6
--When calling interpret (Cond e c1 c2) s it calls the switch function. Calling switch applies the appropriate action to the store, where the choice is determined by its first argument.
interpret (Cond e c1 c2) s = switch (eval e s) (interpret c1) (interpret c2) s
--Problem #7
--When calling interpret (While e body) s it calls switch to distinguishing between the actions taken when the loop test evaluates to 1 and when the loop test evaluates to 0
--The success function creates a state s1 based off of the first interpretation of the loop being; interpret body store. s1 is then used in the iteration of the while loop
interpret (While e body) s = switch (eval e s) success id s 
     where 
       success :: Store -> Store
       success store = let s1 = interpret body store  
                       in interpret (While e body) s1


module Interpreter where

-- union = union (|||), delete = remove (---)
import Parsing
import Data.List

-- Free variables of a term (variables that are not bound by a lambda)
freeVariables ::  Term -> [Var]
freeVariables (Constant _) = []
freeVariables (Variable v) = [v]
freeVariables (Application (a, b)) = (freeVariables a) `union` (freeVariables b)
freeVariables (Lambda (x, b)) = filter (\z -> z == x) (freeVariables b)

-- Checks if a variable is free in a term
isFreeVariableOf :: Var -> Term -> Bool
isFreeVariableOf v t = v `elem` (freeVariables t)

--  Create a new variable name, that is not free in a term 
newVar :: Term -> Var
newVar t = newVar' t "new-var"
newVar' :: Term -> Var -> Var
newVar' t nVar = if isFreeVariableOf nVar t
				 then newVar' t (nVar++"1")
				 else nVar


-- substitute a variable in a term with another term
substitution ::  Term -> Var -> Term -> Term
substitution (Variable term1) var term2
	| term1 == var = term2

substitution (Application (t1,t2)) var term2 =
	Application (substitution t1 var term2, substitution t2 var term2)

substitution (Lambda (x,t)) var term2
	| x /= var && not (isFreeVariableOf x term2) = Lambda (x, substitution t var term2)
	| x /= var = substitution (substitution t x (Variable (newVar (Application (t, term2))))) var term2

substitution term1 _ _ = term1

-- apply parameters to a function
betaConversion ::  Term -> Term
betaConversion (Application (Lambda (x, a), b)) = substitution a x b
betaConversion t = t


-- pattern matching here probably buggy
etaConversion :: Term -> Term
etaConversion (Lambda (x, Application (t, Variable x2)))
	| x == x2 = etaConversion' x t (Lambda (x, Application (t, Variable x2)))

etaConversion te = te

etaConversion' :: Var -> Term -> Term -> Term
etaConversion' _ (Constant (Num t)) _ = Constant (Num t)
etaConversion' x t te = if (isFreeVariableOf x t) then te else t


-- give the constants some meaning
deltaConversion :: Term -> Term
deltaConversion (Application (Constant Succ, Constant (Num n))) = Constant (Num (n + 1))
deltaConversion (Application (Application (Constant Add, Constant (Num n)), Constant (Num m))) = Constant (Num (n + m))
deltaConversion t = t

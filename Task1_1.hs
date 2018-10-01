module Task1_1 where

import Todo(todo)

data Term = IntConstant{ intValue :: Int }           -- числовая константа
            | Variable{ varName :: String }          -- переменная
            | BinaryTerm{ lhv :: Term, rhv :: Term, operation :: Operation } -- бинарная операция
            deriving(Show,Eq)

data Operation = Sum
                 | Diff
                 | Mult
                 deriving (Show, Eq)

-- Для бинарных операций необходима не только реализация, но и адекватные
-- ассоциативность и приоритет	
(|+|) :: Term -> Term -> Term
(|+|) l r = BinaryTerm l r Sum
infixl 5 |+|

(|-|) :: Term -> Term -> Term
(|-|) l r = BinaryTerm l r Diff
infixl 5 |-|

(|*|) :: Term -> Term -> Term
(|*|) l r = BinaryTerm l r Mult
infixl 6 |*|


solveTerm (IntConstant l) (IntConstant r) op  = case op of Sum -> IntConstant(l+r)
                                                           Diff -> IntConstant(l-r)
                                                           Mult -> IntConstant(l*r)
solveTerm l r @(IntConstant rhv) op  = BinaryTerm l r op
solveTerm l @(IntConstant lhv) r op  = BinaryTerm l r op
solveTerm l r op = BinaryTerm l r op


-- Заменить переменную `varName` на `replacement`
-- во всём выражении `expression`
replaceVar :: String -> Term -> Term -> Term
replaceVar varName replacement expression @(BinaryTerm l r op) = 
    let recReplaceVar = replaceVar varName replacement in
    BinaryTerm (recReplaceVar l) (recReplaceVar r) op
replaceVar varName replacement expression @(Variable v) | v == varName = replacement
                                                        | otherwise = Variable v
replaceVar varName replacement expression = expression

-- Посчитать значение выражения `Term`
-- если оно состоит только из констант
evaluate :: Term -> Term
evaluate expression @(BinaryTerm l r op) = solveTerm (evaluate l) (evaluate r) op
evaluate expression = expression
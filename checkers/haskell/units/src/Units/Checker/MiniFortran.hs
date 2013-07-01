module Units.Checker.MiniFortran where

import Data.Generics
import Data.List
import Language.MiniFortran

import Units.Checker
import Units.Constraints

generateConstraints :: Data a => a -> CheckerM ()
generateConstraints pgm = do
    mapM assignment (getAssignments pgm)
    mapM numeric (getNumericExprs pgm)
    return ()

assignment :: (String, NumericExpr) -> CheckerM Int
assignment (x, e) = do
    n <- numeric e
    v <- getOrAdd x
    addConstraint $ [v] :== [n]
    return v

numeric :: NumericExpr -> CheckerM Int
numeric (NumericVal (RefExpr (VarRef x))) = getOrAdd x
numeric (NumericVal (Lit _)) = fresh
numeric (e1 :*: e2) = do
    x1 <- numeric e1
    x2 <- numeric e2
    n <- fresh
    addConstraint $ [n] :== [x1, x2]
    return n
numeric (e1 :+: e2) = do
    x1 <- numeric e1
    x2 <- numeric e2
    n <- fresh
    addConstraint $ [x1] :== [x2]
    addConstraint $ [n] :== [x2]
    return n
numeric (e1 :-: e2) = do
    x1 <- numeric e1
    x2 <- numeric e2
    n <- fresh
    addConstraint $ [x1] :== [x2]
    addConstraint $ [n] :== [x2]
    return n
numeric (e1 :/: e2) = do
    x1 <- numeric e1
    x2 <- numeric e2
    n <- fresh
    addConstraint $ [x1] :== [n, x2]
    return n
numeric (NumericMinus e) = do
    x <- numeric e
    n <- fresh
    addConstraint $ [n] :== [x]
    return n
numeric x = error $ "numeric: " ++ show x

-- TODO ArrayRef?
getAssignments :: Data a => a -> [(String, NumericExpr)]
getAssignments
    = map (\(VarRef s :=: NumE e) -> (s, e))
    . listify isAssignment

-- TODO slow and ugly. Can we use 'everythingBut'?
getNumericExprs :: Data a => a -> [NumericExpr] 
getNumericExprs pgm = map unNumE $ allExprs \\ rhsExprs
  where
    rhsExprs = listify isNumericExpr $ listify isAssignment pgm
    allExprs = listify isNumericExpr pgm

isAssignment (VarRef _ :=: NumE _) = True
isAssignment _ = False

isNumericExpr (NumE _) = True
isNumericExpr _ = False

unNumE (NumE x) = x
unNumE x = error $ "unNumE: " ++ show x

module Units.Constraints.Simplify where

import Data.Generics
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map

import Units.Constraints
import Units.Checker (CheckerState(..))

type RHS = [Int]

-- TODO could be IntMap
type IVMap = Map Int RHS

-- TODO move this
type VarMap = Map String Int

merge :: VarMap -> [Constraint] -> ([(Int, Int)], [Constraint])
merge varMap cs = (eqs, foldr substitute rest eqs)
  where
    (eqs, rest) = partitionWith eq cs 
    eq ([x] :== [y]) = Left (x, y)
    eq x = Right x

substitute :: (Int, Int) -> [Constraint] -> [Constraint]
substitute (x, y) cs = everywhere (mkT replace) cs
  where
    replace u
        | u == x = y
        | otherwise = u

simplify :: CheckerState -> [Constraint]
simplify st = map (\(x :== cs) -> x :== substituteIVs ivmap cs) svcs
  where
    (ivmap, svcs) = splitConstraints st

splitConstraints :: CheckerState -> (IVMap, [Constraint]) 
splitConstraints st = (ivmap, svcs) 
  where
    ivmap = Map.fromList ivcs
    (ivcs, svcs) = partitionWith getIVC (constraints st)
    getIVC c@([x] :== cs)
        | x `elem` ivs = Left (x, cs)
        | otherwise = Right c
    getIVC x = Right x
    ivs = [0 .. freshName st - 1] \\ svs -- intermediate vars
    svs = Map.elems (variableMap st)     -- program/source vars

substituteIVs :: IVMap -> RHS -> RHS
substituteIVs ivmap rhs = subst rhs []
  where
    subst [] rs = rs
    subst (c:cs) rs =
        case Map.lookup c ivmap of
            Nothing -> subst cs (c : rs)
            Just us -> subst (us ++ cs) rs

partitionWith :: (a -> Either b c) -> [a] -> ([b], [c])
partitionWith f = foldr (either left right . f) ([], [])
  where
    left  a ~(l, r) = (a:l, r)
    right a ~(l, r) = (l, a:r)

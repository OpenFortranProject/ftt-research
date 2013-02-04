module Units.Checker where

import Control.Monad.Trans.State
import Data.Map (Map)
import qualified Data.Map as Map

import Units.Constraints

type CheckerM = State CheckerState

data CheckerState = CheckerState
    { constraints :: [Constraint]
    , freshName   :: Int
    , variableMap :: Map String Int
    } deriving (Eq, Ord, Read, Show)

initialState = CheckerState
    { constraints = []
    , freshName   = 0
    , variableMap = Map.empty
    }

runCheckerM :: CheckerM a -> CheckerState
runCheckerM = flip execState initialState

addConstraint :: Constraint -> CheckerM ()
addConstraint c = modify (\s -> s { constraints = c : constraints s })

fresh :: CheckerM Int
fresh = do
    x <- gets freshName
    modify (\s -> s { freshName = freshName s + 1 })
    return x

getOrAdd :: String -> CheckerM Int
getOrAdd x = do
    varMap <- gets variableMap
    case Map.lookup x varMap of
        Just n -> return n
        Nothing -> do
            n <- fresh
            modify (\s -> s { variableMap = Map.insert x n (variableMap s) })
            return n

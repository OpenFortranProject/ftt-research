{-# LANGUAGE DeriveDataTypeable #-}
module Units.Constraints where

import Data.List
import Data.Data
import Data.Typeable

data Constraint
    = [Int] :== [Int]
    deriving (Eq, Ord, Read, Data, Typeable)

instance Show Constraint where
    showsPrec _ (xs :== ys) = showVars xs . showString " = " . showVars ys
      where
        showVars = foldr (.) id . intersperse (showString " + ") . map showVar
        showVar x = showString "x" . shows x

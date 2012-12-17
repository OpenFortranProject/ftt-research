{-# LANGUAGE DeriveDataTypeable #-}
module Language.MiniFortran where

import Data.Map ( Map )
import qualified Data.Map as M
import Data.Typeable

-- | It would be 'nice' to upgrade this to something with
-- type info
type Variable = String

newtype Literal = Literal String -- ^ uninterpreted literal constants
  deriving (Read, Show, Eq, Ord, Typeable)

data NumericExpr 
  = NumericExpr :+: NumericExpr
  | NumericExpr :-: NumericExpr
  | NumericExpr :*: NumericExpr
  | NumericExpr :/: NumericExpr
  | NumericMinus NumericExpr -- ^ unary minus
  | NumericVal   ValueExpr
  deriving (Read, Show, Eq, Ord, Typeable)

data LogicExpr
  = LogicExpr :&&: LogicExpr
  | LogicExpr :||: LogicExpr
  | LogicExpr :^:  LogicExpr
  | Not LogicExpr
  | LogicVal ValueExpr
  -- comparisons:
  | NumericExpr :>:  NumericExpr
  | NumericExpr :<:  NumericExpr
  | NumericExpr :>=: NumericExpr
  | NumericExpr :<=: NumericExpr
  | NumericExpr :==: NumericExpr
  | NumericExpr :!=: NumericExpr
  deriving (Read, Show, Eq, Ord, Typeable)

data Expr
  -- | Arithmetic
  = NumE NumericExpr
  -- | Logical & Comparison, not to be confused with log base e
  | LogE LogicExpr
  deriving (Read, Show, Eq, Ord, Typeable)

data ValueExpr
  = Var    Variable
  | Lit    Literal
  | ArrRef Variable [IndexExpr] -- ^ array name and list of indices
  | Slice  SliceExpr
  | Func   Variable [Expr]
  deriving (Read, Show, Eq, Ord, Typeable)

data IndexExpr
  = IdxExpr  NumericExpr
  | IdxSlice SliceExpr
  deriving (Read, Show, Eq, Ord, Typeable)

data SliceExpr
  = SliceExpr
  { seLower  :: LowerBound
  , seUpper  :: UpperBound
  , seStride :: NumericExpr -- ^ defaults to 1
  }
  deriving (Read, Show, Eq, Ord, Typeable)

data UpperBound
  = UBExpr NumericExpr
  | UpperBound
  deriving (Read, Show, Eq, Ord, Typeable)

data LowerBound
  = LBExpr NumericExpr
  | LowerBound
  deriving (Read, Show, Eq, Ord, Typeable)

-- | a group of statements, not a basic block
type Block = [Expr]

-- | We assume that implicit bounds have been
-- reduced to some cannonical form, eg., lbStep has
-- always been resolved to something.
-- But we still have to deal with the case where
-- the loop is of the form DO ... END DO, meaning
-- it should loop forever. We represent that
-- by having Nothing for the LoopBounds in the Do construct.
data LoopBounds
  = LB
  { lbFrom :: NumericExpr
  , lbTo   :: NumericExpr
  , lbStep :: NumericExpr -- ^ Default is 1
  }
  deriving (Read, Show, Eq, Ord, Typeable)

data Stmt
  = Write String -- ^ What else...
  | Read  String -- ^ What else...
  | If Expr Stmt Stmt
  | Select Expr [(NumericExpr, Block)]
  -- | We assume that all loops have been normalized to this form:
  | Do (Maybe LoopBounds) -- ^ From/to and step by, Nothing is the
                          -- infinite loop case
       Block
  | DoWhile LogicExpr Block -- ^ loops with a conditional expr
  | Call Variable [Expr] -- ^ Procedure/Function calls
  | Variable :=: Expr
  deriving (Read, Show, Eq, Ord, Typeable)

data NumericType
  = TyInt
  | TyFloat
  | TyDouble
  | TyComplex
  deriving (Read, Show, Eq, Ord, Typeable)

data LogicalType
  = TyBool
  deriving (Read, Show, Eq, Ord, Typeable)

data LiteralType
  = TyNumeric NumericType
  | TyLogical LogicalType
  | TyString
  | TyChar
  deriving (Read, Show, Eq, Ord, Typeable)

data ArrayType
  = TyArray Dimension LiteralType
  deriving (Read, Show, Eq, Ord, Typeable)

data DataType
  = TyArr  ArrayType
  | TyLit  LiteralType
  | TyUser UserDefinedType
  deriving (Read, Show, Eq, Ord, Typeable)

data UserDefinedType = UserDefinedType Variable [(Variable, DataType)]
  deriving (Read, Show, Eq, Ord, Typeable)

data ProcType = ProcType [DataType]
  deriving (Read, Show, Eq, Ord, Typeable)

data FuncType = FuncType ([DataType], DataType)
  deriving (Read, Show, Eq, Ord, Typeable)

data Type
  = TyData DataType
  | TyProc ProcType
  | TyFunc FuncType
  deriving (Read, Show, Eq, Ord, Typeable)

data Dimension
  = Dim [(LowerBound,UpperBound)]
  deriving (Read, Show, Eq, Ord, Typeable)

data Decl a
  = Decl
  { declName       :: Variable
  , declType       :: a
  , declAttributes :: [Attribute]
  }
  deriving (Read, Show, Eq, Ord, Typeable)

type VarDecl  = Decl DataType
type ProcDecl = Decl ProcType
type FuncDecl = Decl FuncType

data Def a
  = Def
  { defDecl     :: a
  , defVarDecls :: [VarDecl]
  , defBody     :: Block
  }
  deriving (Read, Show, Eq, Ord, Typeable)

type ProcDef = Def ProcDecl
type FuncDef = Def FuncDecl

data Attribute
  = Pure
  | Elemental
  -- LoPE annotations:
  | Halo [Extent] BoundaryCond
  | Intent
  deriving (Read, Show, Eq, Ord, Typeable)

data Extent = Extent -- TODO: what should this be?
  deriving (Read, Show, Eq, Ord, Typeable)

data BoundaryCond = BoundaryCond -- TODO: what should this be?
  deriving (Read, Show, Eq, Ord, Typeable)

data Intent
  = In
  | Out
  | InOut
  deriving (Read, Show, Eq, Ord, Typeable)

type SymbolTable = Map Variable (Decl Type)

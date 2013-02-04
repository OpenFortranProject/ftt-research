{-# LANGUAGE DeriveDataTypeable #-}
module Language.MiniFortran where

import Data.Map ( Map )
import qualified Data.Map as M
import Data.Data (Data)
import Data.Typeable

type Variable = String

type Literal = String -- ^ uninterpreted literal constants

data NumericExpr 
  = NumericExpr :+: NumericExpr
  | NumericExpr :-: NumericExpr
  | NumericExpr :*: NumericExpr
  | NumericExpr :/: NumericExpr
  | NumericMinus NumericExpr -- ^ unary minus
  | NumericVal   ValueExpr
  deriving (Read, Show, Eq, Ord, Data, Typeable)

infixl 6 :+:, :-:
infixl 7 :*:, :/:

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
  | NumericExpr :/=: NumericExpr
  deriving (Read, Show, Eq, Ord, Data, Typeable)

infixr 3 :&&:, :^:
infixr 2 :||:
infix  4 :>:, :<:, :>=:, :<=:, :==:, :/=:

data Expr
  -- | Arithmetic
  = NumE NumericExpr
  -- | Logical & Comparison, not to be confused with log base e
  | LogE LogicExpr
  deriving (Read, Show, Eq, Ord, Data, Typeable)

data ValueExpr
  = RefExpr Ref
  | Lit    Literal
  | Slice  SliceExpr
  | Func   Variable [Expr]
  deriving (Read, Show, Eq, Ord, Data, Typeable)

data Ref
  = VarRef   Variable
  | ArrayRef Variable [IndexExpr] -- ^ array name and list of indices
  deriving (Read, Show, Eq, Ord, Data, Typeable)

data IndexExpr
  = IdxExpr  NumericExpr
  | IdxSlice SliceExpr
  deriving (Read, Show, Eq, Ord, Data, Typeable)

data SliceExpr
  = SliceExpr
  { seLower  :: LowerBound
  , seUpper  :: UpperBound
  , seStride :: NumericExpr -- ^ defaults to 1
  }
  deriving (Read, Show, Eq, Ord, Data, Typeable)

data UpperBound
  = UBExpr NumericExpr
  | UpperBound
  deriving (Read, Show, Eq, Ord, Data, Typeable)

data LowerBound
  = LBExpr NumericExpr
  | LowerBound
  deriving (Read, Show, Eq, Ord, Data, Typeable)

-- | a group of statements, not a basic block
type Block = [Stmt]

-- | We assume that implicit bounds have been
-- reduced to some cannonical form, eg., lbStep has
-- always been resolved to something.
-- But we still have to deal with the case where
-- the loop is of the form DO ... END DO, meaning
-- it should loop forever. We represent that
-- by having Nothing for the LoopBounds in the Do construct.
data LoopBounds
  = LB
  { lbVar  :: Variable
  , lbFrom :: NumericExpr
  , lbTo   :: NumericExpr
  , lbStep :: NumericExpr -- ^ Default is 1
  }
  deriving (Read, Show, Eq, Ord, Data, Typeable)

data Stmt
  = Write String -- ^ What else...
  | Read  String -- ^ What else...
  | If LogicExpr Block Block
  | Select NumericExpr [(NumericExpr, Block)]
  -- | We assume that all loops have been normalized to this form:
  -- From/to and step by, Nothing is the infinite loop case
  | Do      (Maybe LoopBounds) Block
  | DoWhile LogicExpr          Block -- ^ loops with a conditional expr
  | Call Variable [Expr] -- ^ Procedure/Function calls
  | Ref :=: Expr
  deriving (Read, Show, Eq, Ord, Data, Typeable)

data NumericType
  = TyInt
  | TyFloat
  | TyDouble
  | TyComplex
  deriving (Read, Show, Eq, Ord, Data, Typeable)

data LogicalType
  = TyBool
  deriving (Read, Show, Eq, Ord, Data, Typeable)

data LiteralType
  = TyNumeric NumericType
  | TyLogical LogicalType
  | TyString
  | TyChar
  deriving (Read, Show, Eq, Ord, Data, Typeable)

data ArrayType
  = TyArray Dimension LiteralType
  deriving (Read, Show, Eq, Ord, Data, Typeable)

data DataType
  = TyArr  ArrayType
  | TyLit  LiteralType
  | TyUser UserDefinedType
  deriving (Read, Show, Eq, Ord, Data, Typeable)

data UserDefinedType = UserDefinedType Variable [(Variable, DataType)]
  deriving (Read, Show, Eq, Ord, Data, Typeable)

data ProcType = ProcType [DataType]
  deriving (Read, Show, Eq, Ord, Data, Typeable)

data FuncType = FuncType [DataType] DataType
  deriving (Read, Show, Eq, Ord, Data, Typeable)

data Type
  = TyData DataType
  | TyProc ProcType
  | TyFunc FuncType
  deriving (Read, Show, Eq, Ord, Data, Typeable)

data Dimension
  = Dim [(LowerBound,UpperBound)]
  deriving (Read, Show, Eq, Ord, Data, Typeable)

data Decl a
  = Decl
  { declName       :: Variable
  , declType       :: a
  , declAttributes :: [Attribute]
  }
  deriving (Read, Show, Eq, Ord, Data, Typeable)

type VarDecl  = Decl DataType
type ProcDecl = Decl ProcType
type FuncDecl = Decl FuncType

data Def a
  = Def
  { defDecl     :: a
  , defVarDecls :: [VarDecl]
  , defBody     :: Block
  }
  deriving (Read, Show, Eq, Ord, Data, Typeable)

type ProcDef = Def ProcDecl
type FuncDef = Def FuncDecl

data Attribute
  = Pure
  | Elemental
  -- LoPE annotations:
  | Halo [Extent] BoundaryCond
  | Intent
  deriving (Read, Show, Eq, Ord, Data, Typeable)

data Extent = Extent -- TODO: what should this be?
  deriving (Read, Show, Eq, Ord, Data, Typeable)

data BoundaryCond = BoundaryCond -- TODO: what should this be?
  deriving (Read, Show, Eq, Ord, Data, Typeable)

data Intent
  = In
  | Out
  | InOut
  deriving (Read, Show, Eq, Ord, Data, Typeable)

type SymbolTable = Map Variable (Decl Type)

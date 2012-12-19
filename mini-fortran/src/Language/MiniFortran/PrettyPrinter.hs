{-# LANGUAGE RecordWildCards #-}
module Language.MiniFortran.PrettyPrinter where

import Language.MiniFortran
import Text.PrettyPrint.Leijen 

ppList :: [Doc] -> Doc
ppList xs = parens (hcat (punctuate (comma <> space) xs))

ppNumericExpr :: NumericExpr -> Doc
ppNumericExpr = ppNumericExprPrec 0

plusPrec   = 6
minusPrec  = 6
multPrec   = 7
divPrec    = 7
uMinusPrec = 10 -- should this be lower?
numValPrec = 11 -- what should this be?

ppParens :: Bool -> Doc -> Doc
ppParens True  d = parens d
ppParens False d = d

ppNumericExprPrec :: Int -> NumericExpr -> Doc
ppNumericExprPrec d (n1 :+: n2) = ppParens (d > plusPrec) $
  ppNumericExprPrec (plusPrec + 1) n1 <+> text "+" <+> ppNumericExprPrec (plusPrec + 1) n2
ppNumericExprPrec d (n1 :-: n2) = ppParens (d > minusPrec) $
  ppNumericExprPrec (minusPrec + 1) n1 <+> text "-" <+> ppNumericExprPrec (minusPrec + 1) n2
ppNumericExprPrec d (n1 :*: n2) = ppParens (d > multPrec) $
  ppNumericExprPrec (multPrec + 1) n1 <+> text "*" <+> ppNumericExprPrec (multPrec + 1) n2
ppNumericExprPrec d (n1 :/: n2) = ppParens (d > divPrec) $
  ppNumericExprPrec (divPrec + 1) n1 <+> text "/" <+> ppNumericExprPrec (divPrec + 1) n2
ppNumericExprPrec d (NumericMinus n) = ppParens (d > uMinusPrec) $
  text "-" <+> ppNumericExprPrec (uMinusPrec + 1) n
ppNumericExprPrec d (NumericVal val) =
  ppParens (d > numValPrec) $ ppValueExpr val

ppLogicExpr :: LogicExpr -> Doc
ppLogicExpr = ppLogicExprPrec 0

andPrec      = 3
xorPrec      = 3
orPrec       = 2
gtPrec       = 4
ltPrec       = 4
gePrec       = 4
lePrec       = 4
eqPrec       = 4
neqPrec      = 4
notPrec      = 10
logicValPrec = 11

ppLogicExprPrec :: Int -> LogicExpr -> Doc
ppLogicExprPrec d (l1 :&&: l2)   = ppParens (d > andPrec) $
  ppLogicExprPrec (andPrec + 1) l1 <+> text ".AND." <+> ppLogicExprPrec (andPrec + 1) l2
ppLogicExprPrec d (l1 :||: l2)   = ppParens (d > orPrec) $
  ppLogicExprPrec (orPrec + 1) l1 <+> text ".OR."  <+> ppLogicExprPrec (orPrec + 1) l2
ppLogicExprPrec d (l1 :^:  l2)   = ppParens (d > xorPrec) $
  ppLogicExprPrec (xorPrec + 1) l1 <+> text ".XOR." <+> ppLogicExprPrec (xorPrec + 1) l2
ppLogicExprPrec d (n1 :>:  n2)   = ppParens (d > gtPrec) $ 
  ppNumericExprPrec (gtPrec + 1) n1 <+> text ">"  <+> ppNumericExprPrec (gtPrec + 1) n2
ppLogicExprPrec d (n1 :<:  n2)   = ppParens (d > ltPrec) $
  ppNumericExprPrec (ltPrec + 1) n1 <+> text "<"  <+> ppNumericExprPrec (ltPrec + 1) n2
ppLogicExprPrec d (n1 :>=: n2)   = ppParens (d > gePrec) $
  ppNumericExprPrec (gePrec + 1) n1 <+> text ">=" <+> ppNumericExprPrec (gePrec + 1) n2
ppLogicExprPrec d (n1 :<=: n2)   = ppParens (d > lePrec) $
  ppNumericExprPrec (lePrec + 1) n1 <+> text "<=" <+> ppNumericExprPrec (lePrec + 1) n2
ppLogicExprPrec d (n1 :==: n2)   = ppParens (d > eqPrec) $
  ppNumericExprPrec (eqPrec + 1) n1 <+> text "==" <+> ppNumericExprPrec (eqPrec + 1) n2
ppLogicExprPrec d (n1 :/=: n2)   = ppParens (d > neqPrec) $
  ppNumericExprPrec (neqPrec + 1) n1 <+> text "/=" <+> ppNumericExprPrec (neqPrec + 1) n2
ppLogicExprPrec d (Not l)        = ppParens (d > notPrec) $ text ".NOT." <+> ppLogicExprPrec (notPrec + 1) l
ppLogicExprPrec d (LogicVal val) = ppParens (d > logicValPrec) $ ppValueExpr val

ppExpr :: Expr -> Doc
ppExpr (NumE n) = ppNumericExpr n
ppExpr (LogE l) = ppLogicExpr l

ppValueExpr :: ValueExpr -> Doc
ppValueExpr (RefExpr r)   = ppRef r
ppValueExpr (Lit l)       = ppLiteral l
ppValueExpr (Slice sexpr) = ppSliceExpr sexpr
ppValueExpr (Func v args) = text v <> ppList (map ppExpr args)

ppRef :: Ref -> Doc
ppRef (VarRef v)      = text v
ppRef (ArrayRef v is) = text v <> ppList (map ppIndexExpr is)

ppIndexExpr :: IndexExpr -> Doc
ppIndexExpr (IdxExpr n)      = ppNumericExpr n
ppIndexExpr (IdxSlice sexpr) = ppSliceExpr sexpr

ppSliceExpr :: SliceExpr -> Doc
ppSliceExpr SliceExpr {..} =
           ppLowerBound seLower <>
  colon <> ppUpperBound seUpper <>
  ppOptionalStride seStride
  where
  ppOptionalStride (NumericVal (Lit "1")) = empty
  ppOptionalStride n                      = colon <> ppNumericExpr n

ppUpperBound :: UpperBound -> Doc
ppUpperBound (UBExpr n) = ppNumericExpr n
ppUpperBound UpperBound = empty

ppLowerBound :: LowerBound -> Doc
ppLowerBound (LBExpr n) = ppNumericExpr n
ppLowerBound LowerBound = empty

ppBlock :: Block -> Doc
ppBlock stmts = align (vsep (map ppStmt stmts))

ppStmt :: Stmt -> Doc
ppStmt (Write s)       = text "write (*,*)" <+> text s
ppStmt (Read s)        = text "read (*,*)"  <+> text s
ppStmt (If expr tb fb) =
  text "if" <+> ppLogicExpr expr <+> text "then" <$$>
  indent 3 (ppBlock tb)                          <$$>
  text "else"                                    <$$>
  indent 3 (ppBlock fb)                          <$$>
  text "end if"
ppStmt (Select expr cases) =
  text "select case" <+> parens (ppNumericExpr expr) <$$>
  indent 3 (vsep (map ppCase cases))                 <$$>
  text "end select"
ppStmt (Do Nothing stmts) =
  text "do"                  <$$>
  (indent 3 (ppBlock stmts)) <$$>
  text "end do"
ppStmt (Do (Just bounds) stmts) =
  text "do" <+> ppLoopBounds bounds <$$>
  (indent 3 (ppBlock stmts))        <$$>
  text "end do"
ppStmt (DoWhile expr stmts) =
  text "do while" <+> ppLogicExpr expr <$$>
  (indent 3 (ppBlock stmts))           <$$>
  text "end do"
ppStmt (Call proc args) =
  text "call" <+> text proc <> ppList (map ppExpr args)
ppStmt (v :=: expr) =
  ppRef v <+> text "=" <+> ppExpr expr

ppLiteral :: Literal -> Doc
ppLiteral s = text s

ppCase :: (NumericExpr, Block) -> Doc
ppCase (n, stmts) =
  text "case" <+> parens (ppNumericExpr n) <$$>
  indent 3 (ppBlock stmts)

ppLoopBounds :: LoopBounds -> Doc
ppLoopBounds LB {..} =
  text lbVar <+> text "=" <+> ppNumericExpr lbFrom <> comma <+>
  ppNumericExpr lbTo <> comma <+> ppNumericExpr lbStep

ppNumericType :: NumericType -> Doc
ppNumericType TyInt     = text "integer"
ppNumericType TyFloat   = text "real"
ppNumericType TyDouble  = text "real(8)"
ppNumericType TyComplex = text "complex"

ppLogicalType :: LogicalType -> Doc
ppLogicalType TyBool = text "logical"

ppLiteralType :: LiteralType -> Doc
ppLiteralType (TyNumeric nt) = ppNumericType nt
ppLiteralType (TyLogical lt) = ppLogicalType lt
ppLiteralType TyString       = text "character(*)"
ppLiteralType TyChar         = text "character"

ppArrayType :: ArrayType -> Doc
-- TODO: not done yet, need name + attributes
ppArrayType (TyArray d lt) = ppLiteralType lt <> comma <+> ppDimension d

ppFuncType :: FuncType -> Doc
-- TODO: properly show the return type, it needs to be a
-- var decl the same as the function name
ppFuncType (FuncType args ret) = ppList (map ppDataType args)

ppProcType :: ProcType -> Doc
ppProcType (ProcType [])   = empty
ppProcType (ProcType args) = ppList (map ppDataType args)

ppDimension :: Dimension -> Doc
ppDimension (Dim bounds) =
  text "dimension" <> ppList (map ppBounds bounds)
  where
  ppBounds (lb, ub) = ppLowerBound lb <> colon <> ppUpperBound ub

ppDataType :: DataType -> Doc
ppDataType (TyArr arrty) = ppArrayType arrty -- TODO: need name + attributes
ppDataType (TyLit lt)    = ppLiteralType lt
ppDataType (TyUser ut)   = ppUserDefinedType ut

ppUserDefinedType :: UserDefinedType -> Doc
ppUserDefinedType (UserDefinedType name _) = text name

ppVarDecl :: VarDecl -> Doc
-- TODO: print attributes
ppVarDecl Decl {..} = ppDataType declType <+> text "::" <+> text declName 

ppFuncDecl :: FuncDecl -> Doc
ppFuncDecl Decl {..} =
  text "function" <+> text declName <> parens (ppFuncType declType)

ppProcDecl :: ProcDecl -> Doc
ppProcDecl Decl {..} =
  text "procedure" <+> text declName <> parens (ppProcType declType)

ppFuncDef :: FuncDef -> Doc
ppFuncDef Def {..} =
  ppFuncDecl defDecl                          <$$>
  indent 3 (vsep (map ppVarDecl defVarDecls)) <$$>
  indent 3 (ppBlock defBody)                  <$$>
  text "end function"

ppProcDef :: ProcDef -> Doc
ppProcDef Def {..} =
  ppProcDecl defDecl                          <$$>
  indent 3 (vsep (map ppVarDecl defVarDecls)) <$$>
  indent 3 (ppBlock defBody)                  <$$>
  text "end procedure"

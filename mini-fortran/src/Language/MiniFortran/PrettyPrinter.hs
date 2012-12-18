{-# LANGUAGE RecordWildCards #-}
module Language.MiniFortran.PrettyPrinter where

import Language.MiniFortran
import Text.PrettyPrint.Leijen 

ppNumericExpr :: NumericExpr -> Doc
ppNumericExpr (n1 :+: n2)      = parens (ppNumericExpr n1) <+> text "+" <+> parens (ppNumericExpr n2)
ppNumericExpr (n1 :-: n2)      = parens (ppNumericExpr n1) <+> text "-" <+> parens (ppNumericExpr n2)
ppNumericExpr (n1 :*: n2)      = parens (ppNumericExpr n1) <+> text "*" <+> parens (ppNumericExpr n2)
ppNumericExpr (n1 :/: n2)      = parens (ppNumericExpr n1) <+> text "/" <+> parens (ppNumericExpr n2)
ppNumericExpr (NumericMinus n) = text "-" <+> parens (ppNumericExpr n)
ppNumericExpr (NumericVal val) = ppValueExpr val

ppLogicExpr :: LogicExpr -> Doc
ppLogicExpr (l1 :&&: l2)   = parens (ppLogicExpr l1) <+> text ".AND." <+> parens (ppLogicExpr l2)
ppLogicExpr (l1 :||: l2)   = parens (ppLogicExpr l1) <+> text ".OR."  <+> parens (ppLogicExpr l2)
ppLogicExpr (l1 :^: l2)    = parens (ppLogicExpr l1) <+> text ".XOR." <+> parens (ppLogicExpr l2)
ppLogicExpr (Not l)        = text ".NOT." <+> parens (ppLogicExpr l)
ppLogicExpr (LogicVal val) = ppValueExpr val
ppLogicExpr (n1 :>:  n2)   = parens (ppNumericExpr n1) <+> text ">"  <+> parens (ppNumericExpr n2)
ppLogicExpr (n1 :<:  n2)   = parens (ppNumericExpr n1) <+> text "<"  <+> parens (ppNumericExpr n2)
ppLogicExpr (n1 :>=: n2)   = parens (ppNumericExpr n1) <+> text ">=" <+> parens (ppNumericExpr n2)
ppLogicExpr (n1 :<=: n2)   = parens (ppNumericExpr n1) <+> text "<=" <+> parens (ppNumericExpr n2)
ppLogicExpr (n1 :==: n2)   = parens (ppNumericExpr n1) <+> text "==" <+> parens (ppNumericExpr n2)
ppLogicExpr (n1 :/=: n2)   = parens (ppNumericExpr n1) <+> text "/=" <+> parens (ppNumericExpr n2)

ppExpr :: Expr -> Doc
ppExpr (NumE n) = ppNumericExpr n
ppExpr (LogE l) = ppLogicExpr l

ppValueExpr :: ValueExpr -> Doc
ppValueExpr (RefExpr r)   = ppRef r
ppValueExpr (Lit l)       = ppLiteral l
ppValueExpr (Slice sexpr) = ppSliceExpr sexpr
ppValueExpr (Func v args) = text v <> encloseSep lparen rparen comma (map ppExpr args)

ppRef :: Ref -> Doc
ppRef (VarRef v)      = text v
ppRef (ArrayRef v is) = text v <> encloseSep lparen rparen comma (map ppIndexExpr is)

ppIndexExpr :: IndexExpr -> Doc
ppIndexExpr (IdxExpr n)      = ppNumericExpr n
ppIndexExpr (IdxSlice sexpr) = ppSliceExpr sexpr

ppSliceExpr :: SliceExpr -> Doc
ppSliceExpr SliceExpr {..} =
           ppLowerBound seLower <>
  colon <> ppUpperBound seUpper <>
  colon <> ppNumericExpr seStride

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
  text "call" <+> text proc <> encloseSep lparen rparen comma (map ppExpr args)
ppStmt (v :=: expr) =
  ppRef v <+> text "=" <+> ppExpr expr

ppLiteral :: Literal -> Doc
ppLiteral (Literal s) = text s

ppCase :: (NumericExpr, Block) -> Doc
ppCase (n, stmts) =
  text "case" <+> parens (ppNumericExpr n) <$$>
  indent 3 (ppBlock stmts)

ppLoopBounds :: LoopBounds -> Doc
ppLoopBounds LB {..} =
  text lbVar <+> text "=" <+> ppNumericExpr lbFrom <> comma <+>
  ppNumericExpr lbTo <> comma <+> ppNumericExpr lbStep


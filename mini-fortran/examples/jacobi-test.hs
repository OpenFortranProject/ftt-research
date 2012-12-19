import Language.MiniFortran
import Language.MiniFortran.PrettyPrinter

nconst n = Lit (show n)
vr n = RefExpr $ VarRef n
nvr n = NumericVal $ vr n

toprow = [IdxSlice $ SliceExpr LowerBound UpperBound (NumericVal (nconst 1)), 
          IdxExpr (NumericVal (nconst 1))]
botrow = [IdxSlice $ SliceExpr LowerBound UpperBound (NumericVal (nconst 1)), 
          IdxExpr (NumericVal (vr "n"))]
lcol = [IdxExpr (NumericVal (nconst 1)),
        IdxSlice $ SliceExpr LowerBound UpperBound (NumericVal (nconst 1))]
rcol = [IdxExpr (NumericVal (RefExpr (VarRef "n"))),
        IdxSlice $ SliceExpr LowerBound UpperBound (NumericVal (nconst 1))] 

xim1j = (NumericVal 
                  (RefExpr $ 
                   ArrayRef "x" 
                            [IdxExpr (NumericVal (RefExpr $ VarRef "i") :-: (NumericVal (nconst 1))),
                             IdxExpr (NumericVal (RefExpr (VarRef "j")))]))

xip1j = (NumericVal	  
                  (RefExpr $
                   ArrayRef "x"
                            [IdxExpr (NumericVal (RefExpr $ VarRef "i") :+: (NumericVal (nconst 1))),
                             IdxExpr (NumericVal (RefExpr (VarRef "j")))]))

xijm1 = (NumericVal	  
                  (RefExpr $
                   ArrayRef "x"
                            [IdxExpr (NumericVal (RefExpr (VarRef "i"))),
                             IdxExpr (NumericVal (RefExpr $ VarRef "j") :-: (NumericVal (nconst 1)))
                             ]))

xijp1 = (NumericVal	  
                  (RefExpr $
                   ArrayRef "x"
                            [IdxExpr (NumericVal (RefExpr (VarRef "i"))),
                             IdxExpr (NumericVal (RefExpr $ VarRef "j") :+: (NumericVal (nconst 1)))
                             ]))

varDecls =
  [
    Decl { declName       = "n"
         , declType       = TyLit (TyNumeric TyInt)
         , declAttributes = []
         }
  , Decl { declName       = "x"
         , declType       = TyArr (TyArray (Dim [arrBounds, arrBounds]) (TyNumeric TyFloat))
         , declAttributes = []
         }
  , Decl { declName       = "tmp"
         , declType       = TyArr (TyArray (Dim [arrBounds, arrBounds]) (TyNumeric TyFloat))
         , declAttributes = []
         }
  , Decl { declName       = "i"
         , declType       = TyLit (TyNumeric TyInt)
         , declAttributes = []
         }
  , Decl { declName       = "j"
         , declType       = TyLit (TyNumeric TyInt)
         , declAttributes = []
         }
  , Decl { declName       = "eps"
         , declType       = TyLit (TyNumeric TyFloat)
         , declAttributes = []
         }
  ]
  where arrBounds = (LowerBound, UBExpr (NumericVal (Lit "100")))

body = [
  VarRef "n" :=: (NumE $ NumericVal $ nconst 100),
  VarRef "x" :=: (NumE $ NumericVal $ nconst 0),
  ArrayRef "x" toprow :=: (NumE $ NumericVal $ nconst 100),
  VarRef "eps" :=: (NumE $ NumericVal $ nconst 1),
  DoWhile (NumericVal (RefExpr (VarRef "eps")) :>: (NumericVal (nconst 0.0001))) [
    ArrayRef "tmp" toprow :=: (NumE $ NumericVal (RefExpr $ ArrayRef "x" toprow)),
    ArrayRef "tmp" lcol   :=: (NumE $ NumericVal (RefExpr $ ArrayRef "x" lcol)),
    ArrayRef "tmp" botrow :=: (NumE $ NumericVal (RefExpr $ ArrayRef "x" botrow)),
    ArrayRef "tmp" rcol   :=: (NumE $ NumericVal (RefExpr $ ArrayRef "x" rcol)),
    Do (Just $ LB "i" (NumericVal (nconst 2)) 
                      (NumericVal (RefExpr $ VarRef "n") :-: (NumericVal (nconst 1) ))
                      (NumericVal (nconst 1)) ) [
      Do (Just $ LB "j" (NumericVal (nconst 2)) 
                        (NumericVal (RefExpr $ VarRef "n") :-: (NumericVal (nconst 1) ))
                        (NumericVal (nconst 1)) ) [
        ArrayRef "tmp" [IdxExpr (NumericVal (RefExpr (VarRef "i"))),
                        IdxExpr (NumericVal (RefExpr (VarRef "j")))] :=:
         (NumE ((((xim1j :+: xip1j) :+: xijm1) :+: xijp1) :/: (NumericVal (nconst 4))))
      ]
    ],
    VarRef "eps" :=: (NumE $ NumericVal $
                       Func "sqrt" [NumE $ NumericVal $ Func "sum" [
                                   NumE $ (nvr "x" :-: nvr "tmp") :*: (nvr "x" :-: nvr "tmp") ]]),
    VarRef "x" :=: (NumE $ nvr "tmp")
  ] 
  ]

jacobiDef = Def
  { defDecl = Decl
    { declName       = "jacobi"
    , declType       = ProcType []
    , declAttributes = []
    }
  , defVarDecls = varDecls
  , defBody     = body
  }

s = ppProcDef jacobiDef

main = do
  print s
  return ()

import Language.MiniFortran
import Language.MiniFortran.PrettyPrinter

import Units.Checker
import Units.Checker.MiniFortran
import Units.Constraints.Simplify

-- Example program from MIT paper:
floatType = TyLit (TyNumeric TyFloat)
float n = NumericVal (Lit (show n)) 
var x = NumericVal (RefExpr (VarRef x))

varDecls =
    [ Decl "mass" floatType []
    , Decl "velocity" floatType []
    , Decl "height" floatType []
    , Decl "height" floatType []
    , Decl "kinetic" floatType []
    , Decl "potential" floatType []
    ]

body =
    [ VarRef "kinetic" :=: NumE (float "0.5" :*: var "mass" :*: var "velocity"
        :*: var "velocity")
    , VarRef "potential" :=: NumE (var "mass" :*: var "height" :*: float "9.8")
    , Call "print" [NumE (var "kinetic" :+: var "potential")]
    ]

pgm = Def
    { defDecl = Decl "test" (ProcType []) []
    , defVarDecls = varDecls
    , defBody = body
    }

prettypgm = ppProcDef pgm

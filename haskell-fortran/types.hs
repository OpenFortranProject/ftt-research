module Main
where

import Text.XML.HXT.Arrow
import Data.Map (Map, fromList, toList)
import Debug.Trace
import Data.Maybe

type Id = String

data TypeDef = TypeDef { typeDefId :: Id,
                         typeDefName :: String,
                         typeDefIntrinsic :: Bool
                       } deriving (Show)
                                  
data Block = Block Statements deriving (Show)
data DoBlock = DoBlock Block deriving (Show)
data DoConstruct = DoConstruct DoBlock deriving (Show)

xpTypeDef :: PU TypeDef
xpTypeDef = xpElem "T" $
            xpWrap (\ (typeDefId,typeDefName,typeDefIntrinsic)
                    -> TypeDef typeDefId typeDefName (toBoolean typeDefIntrinsic)
                       ,
                    \td -> (typeDefId td,
                            typeDefName td,
                            (fromBoolean $ typeDefIntrinsic td))) $
            (xpTriple
             (xpAttr "id" xpText) 
             (xpAttr "N" xpText)
             (xpAttr "intrinsic" xpText))
            
instance XmlPickler TypeDef where                 
  xpickle = xpTypeDef

data TypeDefs = TypeDefs [TypeDef] deriving (Show)

xpTypeDefs :: PU TypeDefs
xpTypeDefs = xpElem "intrinsic-T-lst" $
             xpWrap (\ (l) -> TypeDefs l,
                     \ (TypeDefs l) -> l) $
             xpList $
             xpTypeDef

instance XmlPickler TypeDefs where
  xpickle = xpTypeDefs
  
data OpDef = OpDef { opDefId :: Id,
                     opDefName :: String,
                     opDefIntrinsic :: Bool 
                   } deriving (Show)

xpOpDef :: PU OpDef
xpOpDef = xpElem "op" $
          xpWrap (\ (opDefId, opDefName, opDefIntrinsic) ->
                   OpDef opDefId opDefName (toBoolean opDefIntrinsic)
                   ,
                  \op -> ((opDefId op),
                          (opDefName op),
                          (fromBoolean $ opDefIntrinsic op))) $
          xpTriple 
          (xpAttr "id" xpText) 
          (xpAttr "N" xpText) 
          (xpAttr "intrinsic" xpText)
                               
instance XmlPickler OpDef where
  xpickle = xpOpDef

data OpDefs = OpDefs [OpDef] deriving (Show)

xpOpDefs :: PU OpDefs
xpOpDefs = xpElem "intrinsic-op-lst" $
             xpWrap (\ (l) -> OpDefs l,
                     \ (OpDefs l) -> l) $
             xpList $
             xpOpDef
             
instance XmlPickler OpDefs where                 
  xpickle = xpOpDefs

data AssignmentDef = AssignmentDef { assignmentId :: Id,
                                     assignmentName :: String,
                                     assignmentIntrinsic :: Bool 
                                   } deriving (Show)
                     
xpAssignmentDef :: PU AssignmentDef
xpAssignmentDef = xpElem "assgt" $
                  xpWrap (\ (assignmentId,
                             assignmentName,
                             assignmentIntrinsic) -> 
                          (AssignmentDef
                           assignmentId
                           assignmentName
                           (toBoolean assignmentIntrinsic))
                          ,
                          \assignmentDef -> 
                          (assignmentId assignmentDef,
                           assignmentName assignmentDef,
                           (fromBoolean $ assignmentIntrinsic assignmentDef))) $
                  xpTriple 
                  (xpAttr "id" xpText) 
                  (xpAttr "N" xpText)
                  (xpAttr "intrinsic" xpText)
            
instance XmlPickler AssignmentDef where                 
  xpickle = xpAssignmentDef

data AssignmentDefs = AssignmentDefs [AssignmentDef] deriving (Show)

xpAssignmentDefs :: PU AssignmentDefs
xpAssignmentDefs = xpElem "intrinsic-assgt" $
             xpWrap (\ (l) -> AssignmentDefs l,
                     \ (AssignmentDefs l) -> l) $
             xpList $
             xpAssignmentDef
             
instance XmlPickler AssignmentDefs where                 
  xpickle = xpAssignmentDefs

data ProgramUnit = ProgramUnit { programUnitType :: Id,
                                 programUnitImport :: Bool,
                                 programUnitSymbolTable :: SymbolTable,
                                 programUnitStatements :: Statements
                               }
                   deriving (Show)

xpProgramUnit :: PU ProgramUnit
xpProgramUnit = xpElem "program-unit" $
                xpWrap (\ (programUnitType,
                           programUnitImport,
                           programUnitSymbolTable,
                           programUnitStatements) -> 
                        (ProgramUnit
                         programUnitType
                         (toBoolean programUnitImport)
                         programUnitSymbolTable
                         programUnitStatements)
                        ,
                        (\ (ProgramUnit
                            programUnitType
                            programUnitImport
                            programUnitSymbolTable
                            programUnitStatements) -> 
                         (programUnitType,
                          (fromBoolean programUnitImport),
                          programUnitSymbolTable,
                          programUnitStatements))) $
                xp4Tuple 
                (xpAttr "T" xpText)
                (xpAttr "import" xpText)
                xpLocalSymbolTable
                xpStatements
            
instance XmlPickler ProgramUnit where  
  xpickle = xpProgramUnit

data ProgramUnits = ProgramUnits [ProgramUnit] deriving (Show)

xpProgramUnitsArg arg = xpElem arg $
                        xpWrap (\ (l) -> ProgramUnits l,
                                \ (ProgramUnits l) -> l) $
                        xpList $
                        xpProgramUnit

data NameRef = SymRef Id Id
             | GlobalSymRef Id Id
             | GenericRef Id Id deriving (Show)
               
data FunctionNameRef = GenericFunctionNameRef NameRef
                       | SubrNameRef NameRef
                       deriving (Show)

data SymbolRef = SymbolDef { symbolUseId :: Id,
                             symbolRefId :: Id,
                             symbolName :: String
                           } 
                 deriving (Show)
                                      
data SymbolUse = SymbolUse SymbolRef deriving (Show)

data VarExpr = VarExpr { veSymbolUse :: SymbolUse,
                         veSymbolRefs :: Maybe Refs
                       } deriving (Show)

data SectionSubscript = SectionSubscript Expr deriving (Show)

data SectionSubscripts = SectionSubscripts [SectionSubscript] deriving (Show)

data Ref = RefSS SectionSubscripts
         | RefAll
         deriving (Show)

data Refs = Refs [Ref] deriving (Show)

data LiteralExpr = LiteralExpr Id String deriving (Show)

data Op = Op { opRefId :: Id,
               opRefDefId :: Id,
               opRefName :: String
             } deriving (Show)

xpOp :: PU Op
xpOp = xpElem "o" $
       xpWrap (\(ref,def,name) -> Op ref def name,
               \(Op ref def name) -> (ref,def,name)) $
       (xpTriple
        (xpAttr "id" xpText)
        (xpAttr "S" xpText)
        (xpElem "c" xpText))

data FunctionExpr = FunctionExpr deriving (Show)

data Keyword = Keyword String deriving (Show)

data ActualArg = ActualArg { aaKeyword :: Maybe Keyword,
                             aaValue :: Expr }
                 deriving (Show)

unwrapComma pairs = map (\(a,b)-> a) pairs

wrapSep sep l = if ((length l) > 0) then
                  map (\a -> (a,Just sep)) (reverse (tail (reverse l))) ++
                  [((last l),Just "")]
                else
                  []
                  
wrapComma = wrapSep ","
              
data ActualArgs = ActualArgs [ActualArg] deriving (Show)

data ActualArgsParens = ActualArgsParens String ActualArgs String deriving (Show)

xpActualArgs :: PU ActualArgs
xpActualArgs = xpElem "actual-arg-lst" $
               xpWrap (\(pairs) -> ActualArgs (unwrapComma pairs),
                       \(ActualArgs l) -> (wrapComma l)) $
               xpList $
               (xpPair
                xpActualArg
                (xpOption xpText))
               
xpActualArgsParens :: PU ActualArgsParens
xpActualArgsParens = xpWrap (\(p1,l,p2) -> ActualArgsParens p1 l p2,
                             \(ActualArgsParens p1 l p2) -> (p1,l,p2)) $
                     (xpTriple
                      xpText
                      xpActualArgs
                      xpText)

xpKeyword :: PU Keyword
xpKeyword = xpElem "keyword" $
            xpElem "keyword" $
            xpWrap (\(a) -> Keyword a,
                    \(Keyword a) -> a) $
            xpText

xpActualArg :: PU ActualArg
xpActualArg = xpElem "actual-arg" $
              xpWrap (\(keyword,text,value) -> ActualArg keyword value,
                      \(ActualArg keyword value) -> (keyword,Just "=",value)) $
              (xpTriple
               (xpOption xpKeyword)
               (xpOption xpText)
               (xpElem "val" xpExpr))
              
data Expr = ExprVarExpr VarExpr
          | ExprLiteralExpr LiteralExpr
          | ExprBinaryOpExpr Expr Op Expr
          | ExprFunctionExpr Id FunctionNameRef ActualArgsParens
          | ExprInitInteger Int
          deriving (Show)

data EntitySymbolDef = EntitySymbolDef SymbolRef deriving (Show)

xpEntitySymbolDef :: PU EntitySymbolDef
xpEntitySymbolDef = xpElem "obj-N" $
                    xpWrap (\(sdef) -> EntitySymbolDef sdef,
                            \(EntitySymbolDef sdef) -> sdef) $
                    xpSymbolRef
                    
instance XmlPickler EntitySymbolDef where
  xpickle = xpEntitySymbolDef
            
data InitialExpression = InitialExpression Expr deriving (Show)

data EntityDecl = EntityDecl { edSymbol :: EntitySymbolDef,
                               edInitial :: Maybe InitialExpression }
                deriving (Show)

xpInitialExpression :: PU InitialExpression
xpInitialExpression = xpElem "init-E" $
                      xpWrap (\(expr) -> InitialExpression expr,
                              \(InitialExpression expr) -> expr) $ 
                      xpExpr

xpEntityDecl :: PU EntityDecl
xpEntityDecl = xpElem "entity-decl" $
               xpWrap (\(esdef,text,initial) -> EntityDecl esdef initial,
                       \(EntityDecl esdef initial) -> (esdef,Nothing,initial)) $
               (xpTriple 
                xpEntitySymbolDef
                (xpOption xpText)
                (xpOption xpInitialExpression))
               

data EntityDecls = EntityDecls [EntityDecl] deriving (Show)

xpEntityDecls :: PU EntityDecls
xpEntityDecls = xpElem "entity-decl-lst" $
                xpWrap (\ (pairs) -> EntityDecls (unwrapComma pairs),
                        \ (EntityDecls l) -> (wrapComma l)) $
                xpList $
                (xpPair
                 xpEntityDecl
                 (xpOption xpText))

data Iterator = Iterator { var :: VarExpr,
                           start :: Expr,
                           end :: Expr
                         } deriving (Show)

data Statements = Statements [Statement] deriving (Show)

xpStatementsArg :: String -> PU Statements
xpStatementsArg name =
  xpElem name  $
             xpWrap (\ (l) -> Statements l,
                     \ (Statements l) -> l) $
             xpList $
             xpStatement

xpStatements = xpStatementsArg "stmt-lst"  
             
xpSymbolRef :: PU SymbolRef
xpSymbolRef = xpElem "s" $
              xpWrap (\(useId,refId,name) -> SymbolDef useId refId name,
                      \(SymbolDef useId refId name) -> (useId,refId,name)) $
              (xpTriple
               (xpAttr "id" xpText)
               (xpAttr "S" xpText)
               (xpElem "c" $ xpText))
                       
xpFunctionNameRef :: PU FunctionNameRef
xpFunctionNameRef = xpAlt tag ps
                    where
                      tag (GenericFunctionNameRef _) = 0
                      tag (SubrNameRef _) = 1
                      ps = [ xpElem "generic" $
                             xpWrap (\(nr) -> GenericFunctionNameRef nr,
                                     \(GenericFunctionNameRef nr) -> nr) $
                             xpNameRef
                           ,
                             xpElem "subr" $
                             xpWrap (\(nr) -> SubrNameRef nr,
                                     \(SubrNameRef nr) -> nr) $
                             xpNameRef
                           ]

data SubrName = SubrName SymbolRef deriving (Show)

xpSubrName :: PU SubrName
xpSubrName = xpElem "subr-N" $
             xpWrap (\(symDef) -> SubrName symDef,
                     \(SubrName symDef) -> symDef) $
             xpSymbolRef

data DummyArgN = DummyArgN SymbolRef deriving (Show)

xpDummyArgN :: PU DummyArgN
xpDummyArgN = xpElem "dummy-arg-N" $
              xpWrap (\(val) -> DummyArgN val,
                      \(DummyArgN val) -> val) $
              xpSymbolRef

data DummyArg = DummyArg { dummyArgId :: Id,
                           dummyArgType :: String,
                           dummyArgN :: DummyArgN
                         } deriving (Show)

data DummyArgSymbol = DummyArgSymbol { dummyArgSymbol :: Id } deriving (Show)

xpDummyArg :: PU DummyArg
xpDummyArg = xpElem "dummy-arg" $ 
             xpWrap (\(id,typ,argn) -> DummyArg id typ argn,
                     \(DummyArg id typ argn) -> (id,typ,argn)) $
             (xpTriple
              (xpAttr "id" xpText)
              (xpAttr "T" xpText)
              xpDummyArgN)

xpDummyArgSymbol :: PU DummyArgSymbol
xpDummyArgSymbol = xpElem "ref" $ 
                   xpWrap (\(val) -> DummyArgSymbol val,
                           \(DummyArgSymbol val) -> (val)) $
                   xpAttr "val" xpText

data DummyArgs = DummyArgs [DummyArg] deriving (Show)

data DummyArgsSymbol = DummyArgsSymbol [DummyArgSymbol] deriving (Show)

xpDummyArgs :: PU DummyArgs
xpDummyArgs = xpElem "dummy-arg-N-lst"  $
              xpWrap (\ (pairs) -> DummyArgs (unwrapComma pairs),
                      \ (DummyArgs l) -> (wrapComma l)) $
              xpList $
              (xpPair 
               xpDummyArg
               (xpOption xpText))

xpDummyArgsSymbol :: PU DummyArgsSymbol
xpDummyArgsSymbol = xpElem "dummy-arg-N-lst"  $
                    xpWrap (\ (l) -> DummyArgsSymbol l,
                            \ (DummyArgsSymbol l) -> (l)) $
                    xpList $
                    xpDummyArgSymbol

data KindSelector = KindSelector VarExpr deriving (Show)

data TSpec = TSpec { tSpecTypeRefId :: Id,
                     tSpecTypeName :: String,
                     tSpecKindSelector :: Maybe KindSelector
                   } deriving (Show)
                              
xpKindSelector :: PU KindSelector
xpKindSelector = xpElem "kind-selector" $
                 xpWrap (\(varExpr) -> KindSelector varExpr,
                         \(KindSelector varExpr) -> (varExpr)) $
                 xpVarExpr

xpTSpec :: PU TSpec
xpTSpec = xpElem "T-spec" $
          xpWrap (\(id,typeName,ks) -> TSpec id typeName ks,
                  \(TSpec id typeName ks) -> (id,typeName,ks)) $
          (xpTriple
           (xpAttr "T" (xpText))
           xpText
           (xpOption xpKindSelector))

data TypeDeclSpec = TypeDeclSpec TSpec deriving (Show)

xpTypeDeclSpec = xpElem "T-spec" $
                 xpWrap (\(tspec) -> TypeDeclSpec tspec,
                         \(TypeDeclSpec tspec) -> (tspec)) $
                 xpTSpec
                 
data Intent = In | Out | InOut deriving (Show)

xpIntent :: PU Intent
xpIntent = xpAlt tag ps
           where
             tag (In) = 0
             tag (Out) = 1
             tag (InOut) = 2
             ps = [ xpWrap (\() -> In,
                            \(In) -> ()) $
                    xpAttrFixed "N" "intent(in)"
                  ,
                    xpWrap (\() -> Out,
                            \(Out) -> ()) $
                    xpAttrFixed "N" "intent(out)"
                  ,
                    xpWrap (\() -> InOut,
                            \(InOut) -> ()) $
                    xpAttrFixed "N" "intent(inout)"
                  ]
                  
xpSymbolUse :: PU SymbolUse
xpSymbolUse = xpElem "S" $
              xpWrap (\(sd) -> SymbolUse sd,
                      \(SymbolUse sd) -> (sd)) $
              xpSymbolRef
              
xpSectionSubscript :: PU SectionSubscript
xpSectionSubscript = xpElem "SC-SS" $
                     xpElem "SS" $
                     xpWrap (\(expr) -> SectionSubscript expr,
                             \(SectionSubscript expr) -> (expr)) $
                     xpExpr

xpSectionSubscripts :: PU SectionSubscripts
xpSectionSubscripts = xpElem "SC-SS-lst" $
                      xpWrap (\(pairs) -> SectionSubscripts (unwrapComma pairs),
                              \(SectionSubscripts l) -> (wrapComma l)) $
                      xpList $
                      (xpPair
                       xpSectionSubscript
                       (xpOption xpText))

xpRef :: PU Ref
xpRef = xpElem "ref" $
        xpAlt tag ps
          where
            tag (RefSS _) = 0
            tag (RefAll) = 1
            ps = [ xpWrap (\((),po,subscripts,pc) -> RefSS subscripts,
                           \(RefSS subscripts) -> ((),"(",subscripts,")")) $
                   (xp4Tuple
                    (xpAttrFixed "T" "array-element")
                    xpText
                    xpSectionSubscripts
                    xpText)
                 ,
                   xpWrap (\(()) -> RefAll,
                           \(RefAll) -> ()) $
                   xpAttrFixed "T" "array-full"
                 ]

xpRefs :: PU Refs
xpRefs = xpElem "ref-lst" $
         xpWrap (\(l) -> Refs l,
                 \(Refs l) -> (l)) $
         xpList $
         xpRef

xpVarExpr :: PU VarExpr
xpVarExpr = xpElem "E" $
            xpWrap (\((),sym,refs) -> VarExpr sym refs,
                    \(VarExpr sym refs) -> ((),sym,refs)) $
            (xpTriple
             (xpAttrFixed "T" "var")
             xpSymbolUse
             (xpOption xpRefs))

data UpperBound = UpperBound VarExpr deriving (Show)

xpUpperBound :: PU UpperBound
xpUpperBound = xpElem "upper-bound" $
               xpWrap (\(varExpr) -> UpperBound varExpr,
                       \(UpperBound varExpr) -> varExpr) $
               xpVarExpr

data ShapeSpec = ShapeSpec { ssUpperBound :: UpperBound
                           }
                 | Deferred
                 | Unknown
                 deriving (Show)
                                      

xpShapeSpec :: PU ShapeSpec
xpShapeSpec = xpElem "shape-spec" $
              xpAlt tag ps
                where
                  tag (ShapeSpec upper) = 0
                  tag (Deferred) = 1
                  ps = [ xpWrap (\(upper) -> ShapeSpec upper ,
                                 \(ShapeSpec upper) -> upper) $
                         xpUpperBound
                       ,
                         xpWrap (\(text) -> 
                                  if text == ":" then
                                    Deferred
                                  else
                                    Unknown,
                                 \(Deferred) -> (":")) $
                         xpText 
                       ]

data ShapeSpecs = ShapeSpecs [ShapeSpec] deriving (Show)

xpShapeSpecs :: PU ShapeSpecs
xpShapeSpecs = xpElem "shape-spec-lst" $
                 xpWrap (\(pairs) -> ShapeSpecs (unwrapComma pairs),
                           \(ShapeSpecs l) -> (wrapComma l)) $
                 xpList $
                 (xpPair
                   xpShapeSpec
                   (xpOption xpText))

data ArraySpecDef = ArraySpecDef { arraySpecDef :: Id,
                                   arraySpecShapeDescription :: String,
                                   arraySpecShapeSpecs :: ShapeSpecs
                                 }
                  deriving (Show)

xpArraySpecDef :: PU ArraySpecDef
xpArraySpecDef = xpElem "array-spec" $
                 xpWrap (\(id,desc,t1,shapes,t2)
                         -> ArraySpecDef id desc shapes,
                         \(ArraySpecDef id desc shapes)
                         -> (id,desc,"(",shapes,")"))
                 (xp5Tuple
                  (xpAttr "id" xpText)
                  (xpAttr "shape" xpText)
                  xpText
                  xpShapeSpecs
                  xpText
                 )

data ArraySpec = ArraySpec ArraySpecDef deriving (Show)

xpArraySpec :: PU ArraySpec
xpArraySpec = xpElem "array-spec" $
              xpWrap (\(def) -> ArraySpec def,
                      \(ArraySpec def) -> def) $
              xpArraySpecDef

data AttrSpecDecl = IntentSpec { intentSpecId :: Id,
                                 intentSpecIntent :: Intent,
                                 intentSpecText :: String
                               }
                  | DimensionSpec { dimensionSpecId :: Maybe Id,
                                    dimensionSpecArraySpec :: ArraySpec }
                  | Allocatable
                  deriving (Show)

xpAttrSpecDecl :: PU AttrSpecDecl
xpAttrSpecDecl = xpElem "attr-spec" $
                 xpAlt tag ps
                   where
                     tag (IntentSpec _ _ _) = 0
                     tag (DimensionSpec _ _) = 1
                     tag (Allocatable) = 2
                     ps = [ xpWrap (\ (id,intent,str)
                                    -> IntentSpec id intent str,
                                    \ (IntentSpec id intent str)
                                    -> (id,intent,str)) $
                            (xpTriple
                             (xpAttr "id" xpText)
                             xpIntent
                             xpText)
                          ,
                            xpWrap (\ (id,dimension,str,arraySpec)
                                      -> DimensionSpec id arraySpec,
                                    \ (DimensionSpec id arraySpec)
                                    -> (id,(),"dimension",arraySpec)) $
                            (xp4Tuple
                             (xpOption (xpAttr "id" xpText))
                             (xpAttrFixed "N" "dimension")
                             xpText
                             xpArraySpec)
                          ,
                            xpWrap (\() -> Allocatable,
                                    \(Allocatable) -> ()) $
                            (xpAttrFixed "N" "allocatable")
                          ]
                            
data AttrSpecsDecl = AttrSpecsDecl [AttrSpecDecl] deriving (Show)

xpAttrSpecsDecl :: PU AttrSpecsDecl
xpAttrSpecsDecl = xpElem "attr-spec-lst" $
                  xpWrap (\(pairs) -> AttrSpecsDecl $ unwrapComma pairs,
                          \(AttrSpecsDecl l) -> wrapComma l) $
                  xpList $
                  (xpPair
                   xpAttrSpecDecl
                   (xpOption xpText))

data Renames = Renames [RenameType] deriving (Show)

data Statement = Subr { subrName :: SubrName,
                        subrDummyArgs :: DummyArgs
                      }
                 | EndSubr { endSubrText :: String }
                 | TypeDeclAttr { typeDeclSpec :: TypeDeclSpec,
                                  typeDeclAttrSpecs :: AttrSpecsDecl,
                                  typeDeclEntityDecls :: EntityDecls
                                }
                 | TypeDeclNoAttr { typeDeclSpec :: TypeDeclSpec,
                                    typeDeclEntityDecls :: EntityDecls
                                  }
                 | StatementDoConstruct DoBlock
                 | StatementDo Iterator
                 | StatementBlock Statements
                 | AssignmentStatement VarExpr Assignment Expr
                 | StatementModule String
                 | StatementEndModule String
                 | StatementUse String Renames
                 | StatementImplicitNone
                 | StatementSave
                 | StatementPrivate
                 | StatementPublic AccessIds
                 | StatementInterfaceGeneric FunctionNameRef
                 | StatementInterfaceEnd
                 | StatementContains
                 | StatementIf Expr Statement
                 | StatementCall { scSubr :: Maybe Id,
                                   scNameRef :: FunctionNameRef,
                                   scActualArgs :: ActualArgsParens }
                 | StatementModuleProc ProcNameRefs
                 | InterfaceBlock { ibStatement :: Statement,
                                    ibLC1 :: Maybe LinesOrComments,
                                    ibSpecList :: Statements,
                                    ibLC2 :: Maybe LinesOrComments,
                                    ibEndStatement :: Statement }
                 | InternalSubProgramPart Statement
                 | InternalSubPrograms ProgramUnits
                 | StatementLineOrComment LineOrComment
                 deriving (Show)

xpDoBlock :: PU DoBlock
xpDoBlock = xpElem "do-block" $
            xpWrap (\(block) -> DoBlock block,
                    \(DoBlock block) -> (block)) $
            xpBlock
            
xpExpr :: PU Expr            
xpExpr = xpAlt tag ps
           where 
             tag (ExprVarExpr _)  = 0
             tag (ExprLiteralExpr _) = 1
             tag (ExprBinaryOpExpr _ _ _) = 2
             tag (ExprFunctionExpr _ _ _) = 3
             ps = [ xpWrap (\obj -> (ExprVarExpr obj),
                            \(ExprVarExpr obj) -> obj) $
                    xpVarExpr
                    ,
                    xpElem "E" $ 
                    xpWrap (\((),val,typeRef,val2) 
                            -> ExprLiteralExpr (LiteralExpr typeRef val), 
                            \(ExprLiteralExpr (LiteralExpr typeRef val))
                            -> ((),val,typeRef,val)) $
                    (xp4Tuple
                     (xpAttrFixed "T" "literal")
                     (xpAttr "val" xpText)
                     (xpAttr "CST-T" xpText)
                     xpText)
                  , 
                    xpElem "E" $ 
                    xpWrap (\((),op1,op,op2) -> ExprBinaryOpExpr op1 op op2,
                            \(ExprBinaryOpExpr op1 op op2) -> ((),op1,op,op2)) $
                    (xp4Tuple
                     (xpAttrFixed "T" "op")
                     (xpElem "op1" xpExpr)
                     (xpElem "op" xpOp)
                     (xpElem "op2" xpExpr))
                    ,
                    xpElem "E" $
                    xpWrap (\((),fref,nr,pal) -> ExprFunctionExpr fref nr pal,
                            \(ExprFunctionExpr fref nr pal) -> ((),fref,nr,pal)) $
                    (xp4Tuple
                     (xpAttrFixed "T" "fct-ref")
                     (xpAttr "fct" xpText)
                     xpFunctionNameRef
                     xpActualArgsParens
                    )
                  ]

xpVar :: PU VarExpr 
xpVar = xpElem "var" $ xpVarExpr

xpIterator :: PU Iterator
xpIterator = xpElem "iterator" $ 
             xpElem "iterator" $
             xpWrap (\(t1,varExpr,t2,start,t3,end) -> Iterator varExpr start end,
                     \(Iterator varExpr start end) -> ("do",varExpr,"=",start,",",end)) $
             (xp6Tuple
              xpText
              xpVar
              xpText
              (xpElem "start" xpExpr)
              xpText
              (xpElem "end" xpExpr))

xpBlock :: PU Block
xpBlock = xpElem "block" $
          xpWrap (\(sl) -> Block sl,
                  \(Block sl) -> (sl)) $
          xpStatements

data RenameType = RTContFree String
                  | RTRename {
                    rtFrom :: NameRef,
                    rtTo :: Maybe NameRef
                    }
                  | RTL deriving (Show)
                               
xpNameRef :: PU NameRef
xpNameRef = xpAlt tag ps
            where 
              tag (SymRef _ _) = 0
              tag (GlobalSymRef _ _) = 1
              tag (GenericRef _ _) = 2
              getSym = (xpTriple -- assume -xml-defn
                        (xpAttr "id" xpText)
                        (xpAttr "S" xpText)
                        (xpElem "c" xpText))
              ps = [ xpElem "s" $
                     xpWrap (\(a,s,adup) -> SymRef a s,
                             \(SymRef a s) -> (a,s,a)) $ getSym
                   ,
                     xpElem "gs" $
                     xpWrap (\(a,s,adup) -> GlobalSymRef a s,
                             \(GlobalSymRef a s) -> (a,s,a)) $ getSym
                   ,
                     xpElem "g" $
                     xpWrap (\(a,s,adup) -> GenericRef a s,
                             \(GenericRef a s) -> (a,s,a)) $ getSym
                   ]
                     
xpRenameType :: PU RenameType
xpRenameType = xpAlt tag ps
  where
    tag (RTRename _ _) = 0
    tag (RTContFree _) = 1
    tag (RTL) = 2
    ps = [ xpElem "rename" $
           xpWrap (\(lN,useN) -> RTRename lN useN,
                   \(RTRename lN useN) -> (lN,useN)) $
           (xpPair 
            (xpElem "use-N" xpNameRef)
            (xpOption (xpElem "l-N" xpNameRef)))
         ,
            xpElem "cont-free" $
            xpWrap (\(obj) -> RTContFree obj,
                    \(RTContFree obj) -> (obj)) $
            xpText
         ,
            xpElem "L" $
            xpWrap (\(()) -> RTL,
                    \(RTL) -> ()) $
            xpUnit
         ]
                     
xpRenames :: PU Renames
xpRenames = xpElem "rename-lst" $
            xpWrap (\(pairs) -> Renames (unwrapComma pairs),
                    \(Renames l) -> (wrapSep "" l)) $
            xpList $
            (xpPair
             xpRenameType
             (xpOption xpText))

data AccessIds = AccessIds [NameRef] deriving (Show)

xpAccessIds :: PU AccessIds
xpAccessIds = xpElem "access-id-lst" $
              xpWrap (\(l) -> AccessIds l,
                      \(AccessIds l) -> (l)) $
              xpList $
              xpNameRef 
              
data ProcNameRefs = ProcNames [NameRef] deriving (Show)
              
xpProcNameRefs :: PU ProcNameRefs
xpProcNameRefs = xpElem "proc-N-lst" $ 
                 xpWrap (\(l) -> ProcNames l,
                         \(ProcNames l) -> (l)) $
                 xpList $
                 xpNameRef

data LineOrComment = L | C String deriving (Show)
                            
xpLineOrComment :: PU LineOrComment
xpLineOrComment = xpAlt tag ps
                  where
                    tag (L) = 0
                    tag (C _) = 1
                    ps = [ xpElem "L" $
                           xpWrap (\() -> (L),
                                   \(L) -> ()) $
                           xpUnit
                         ,
                           xpElem "C" $
                           xpWrap (\(text) -> (C text),
                                   \(C text) -> (text)) $
                           xpText                  
                         ]

data LinesOrComments = LinesOrComments [LineOrComment] deriving (Show)
                  
xpLinesOrComments :: PU LinesOrComments
xpLinesOrComments = xpWrap (\(l) -> LinesOrComments l,
                            \(LinesOrComments l) -> (l)) $
                    xpList $
                    xpLineOrComment

xpStatement :: PU Statement
xpStatement = xpAlt tag ps
              where 
                tag (Subr _ _) = 0
                tag (EndSubr _) = 1
                tag (TypeDeclAttr _ _ _) = 2
                tag (TypeDeclNoAttr _ _) = 3
                tag (StatementDoConstruct _) = 4
                tag (StatementDo _) = 5
                tag (StatementBlock _) = 6
                tag (AssignmentStatement _ _ _) = 7
                tag (StatementModule _) = 8
                tag (StatementEndModule _) = 9
                tag (StatementUse _ _) = 10
                tag (StatementImplicitNone) = 11
                tag (StatementSave) = 12
                tag (StatementPrivate) = 13
                tag (StatementPublic _) = 14
                tag (StatementInterfaceGeneric _) = 15
                tag (StatementInterfaceEnd) = 16
                tag (StatementModuleProc _) = 17
                tag (StatementContains) = 18
                tag (StatementIf _ _) = 19
                tag (StatementCall _ _ _) = 20
                tag (InterfaceBlock _ _ _ _ _) = 21
                tag (InternalSubProgramPart _) = 22
                tag (InternalSubPrograms _) = 23
                tag (StatementLineOrComment _) = 24
                ps = [ xpElem "stmt" $ -- Subr, 0
                       xpWrap (\(attr,t1,sr,t2,dl,t3) -> Subr sr dl,
                               \(Subr sr dl) -> ((),"subroutine",sr,"(",dl,")")) $ 
                       (xp6Tuple
                        (xpAttrFixed "T" "subr")
                        xpText
                        xpSubrName
                        xpText -- (
                        xpDummyArgs
                        xpText -- )
                       )
                     ,
                       xpElem "stmt" $ -- EndSubr 1
                       xpWrap (\((),text) -> EndSubr text,
                               \(EndSubr text) -> ((),text)) $
                       (xpPair
                        (xpAttrFixed "T" "end-subr")
                        xpText)
                     ,
                       xpElem "stmt" $ -- TypeDeclAttr 2
                       xpWrap (\(attr,tdi, t1, tdas, t2, tded) -> 
                                TypeDeclAttr tdi tdas tded,
                               \(TypeDeclAttr tdi tdas tded) -> 
                               ((),tdi, ",", tdas, "::", tded)) $
                       (xp6Tuple
                        (xpAttrFixed "T" "T-decl")
                        xpTypeDeclSpec
                        xpText
                        xpAttrSpecsDecl
                        xpText
                        xpEntityDecls)
                     ,
                       xpElem "stmt" $ -- TypeDeclNoAttr 3
                       xpWrap (\(attr,tdi, t1, tded) -> 
                                TypeDeclNoAttr tdi tded,
                               \(TypeDeclNoAttr tdi tded) -> 
                               ((),tdi, "::", tded)) $
                       (xp4Tuple
                        (xpAttrFixed "T" "T-decl")
                        xpTypeDeclSpec
                        xpText
                        xpEntityDecls)
                     ,
                       xpElem "do-construct" $ -- DoConstruct 4
                       xpWrap (\(doBlock,line,pair) -> StatementDoConstruct doBlock,
                               \(StatementDoConstruct doBlock) -> (doBlock,Just (),((),"end do")))$
                       (xpTriple
                        xpDoBlock
                        (xpOption (xpElem "L" (xpUnit)))
                        (xpElem "end-do-stmt"
                                    (xpElem "stmt"
                                                (xpPair (xpAttrFixed "T" "end-do") xpText))))
                     ,
                       xpElem "stmt" $ -- Do Statement 5
                       xpWrap (\(attr,iterator) -> StatementDo iterator,
                               \(StatementDo iterator) -> ((),iterator)) $
                       (xpPair 
                        (xpAttrFixed "T" "do")
                        xpIterator)
                     ,
                       xpElem "block" $ -- Block 6
                       xpWrap (\(sl) -> StatementBlock sl,
                               \(StatementBlock sl) -> (sl)) $
                       xpStatements
                     ,
                       xpElem "stmt" $ -- AssignmentStatement, 7
                       xpWrap (\((),lhs,aop,rhs) -> 
                                AssignmentStatement lhs aop rhs,
                               \(AssignmentStatement lhs aop rhs) ->
                               ((),lhs,aop,rhs)) $
                       (xp4Tuple
                        (xpAttrFixed "T" "assgt")
                        xpVar
                        xpAssignment
                        (xpElem "E" xpExpr))
                       ,
                       xpElem "stmt" $ -- Module, 8
                       xpWrap (\((),text,(name,nameDup)) -> StatementModule name,
                               \(StatementModule name) -> ((),"module ",(name,name))) $
                       (xpTriple
                        (xpAttrFixed "T" "module")
                        xpText
                        (xpElem "module-N" $ xpElem "s" $ 
                         (xpPair
                          (xpAttr "N" xpText)
                          (xpElem "c" $ xpText))))
                     ,
                       xpElem "stmt" $ -- EndModule, 9
                       xpWrap (\((),text) -> StatementEndModule text,
                               \(StatementEndModule text) -> ((),text)) $
                       (xpPair
                        (xpAttrFixed "T" "end-module")
                        xpText)
                     ,
                       xpElem "stmt" $ -- Use, 10
                       xpWrap (\((),t1,(n1,n2),t2,rl)
                               -> StatementUse n1 rl,
                               \(StatementUse n1 rl) -> 
                               ((),"use ",(n1,n1),", only : ",rl)) $
                       (xp5Tuple
                        (xpAttrFixed "T" "use")
                        xpText
                        (xpElem "module-N" $ (xpElem "s" $ 
                                              (xpPair
                                               (xpAttr "N" xpText)
                                               (xpElem "c" $ xpText))))
                        xpText
                        xpRenames)
                     ,
                       xpElem "stmt" $ -- ImplicitNone 11
                       xpWrap (\() -> StatementImplicitNone,
                               \(StatementImplicitNone) -> ()) $
                       xpAttrFixed "T" "implicit-none"
                     ,
                       xpElem "stmt" $ -- Save 12
                       xpWrap (\((),text) -> StatementSave,
                               \(StatementSave) -> ((),"implicit none")) $
                       (xpPair 
                        (xpAttrFixed "T" "save")
                        xpText)
                     ,
                       xpElem "stmt" $ -- Private 13
                       xpWrap (\((),text) -> StatementPrivate,
                               \(StatementPrivate) -> ((),"private")) $
                       (xpPair
                        (xpAttrFixed "T" "private")
                        xpText)
                     ,
                       xpElem "stmt" $ -- Public 14
                       xpWrap (\((),text,al) -> StatementPublic al,
                               \(StatementPublic al) -> ((),"public",al)) $
                       (xpTriple
                        (xpAttrFixed "T" "public")                     
                        xpText
                        xpAccessIds)
                     ,
                       xpElem "stmt" $ -- 15 (private to intf-block)
                       xpWrap (\((),(),t1,nameRef) ->
                                StatementInterfaceGeneric nameRef,
                               \(StatementInterfaceGeneric nameRef) ->
                               ((),(),"interface ",nameRef)) $
                       (xp4Tuple
                        (xpAttrFixed "T" "intf")
                        (xpAttrFixed "intf-T" "generic")
                        xpText
                        xpFunctionNameRef)
                     ,
                       xpElem "stmt" $ -- 16 (private to intf-block)
                       xpWrap (\((),text)
                               -> StatementInterfaceEnd,
                               \(StatementInterfaceEnd)
                               -> ((), "end interface")) $
                       (xpPair
                        (xpAttrFixed "T" "end-intf")
                        xpText)
                     ,
                       xpElem "stmt" $ -- 17                       
                       xpWrap (\((),text,procLst) -> 
                                StatementModuleProc procLst,
                               \(StatementModuleProc procLst)
                               -> ((),"module procedure ",procLst)) $
                       (xpTriple
                        (xpAttrFixed "T" "module-proc")
                        xpText
                        xpProcNameRefs)
                     ,
                       xpElem "stmt" $ -- 18 (private to internal-subprogram-part)
                       xpWrap (\((),text) -> StatementContains,
                               \(StatementContains) -> ((),"contains")) $
                       (xpPair
                        (xpAttrFixed "T" "contains")
                        xpText)
                     ,
                       xpElem "stmt" $ -- 19
                       xpWrap (\((),t1,condition,t2,action) -> StatementIf condition action,
                               \(StatementIf condition action) -> ((),"if (",condition,")",action)) $
                       (xp5Tuple
                         (xpAttrFixed "T" "if")
                         xpText
                         (xpElem "E" $ xpExpr)
                         xpText
                         (xpElem "action-stmt" xpStatement))
                     ,
                       xpElem "stmt" $ -- 20
                       xpWrap (\((),sattr,t1,subr,pal) -> StatementCall sattr subr pal,
                               \(StatementCall sattr subr pal) -> ((),sattr,"call",subr,pal)) $
                       (xp5Tuple
                        (xpAttrFixed "T" "call")
                        (xpOption (xpAttr "subr" xpText))
                        xpText
                        xpFunctionNameRef
                        xpActualArgsParens)
                     ,
                       xpElem "intf-block" $ -- 21
                       xpWrap (\(st,lc1,sl,lc2,end) -> 
                                InterfaceBlock st lc1 sl lc2 end,
                               \(InterfaceBlock st lc1 sl lc2 end) -> 
                               (st,lc1,sl,lc2,end)) $
                       (xp5Tuple
                        (xpElem "intf-stmt" $ xpStatement)
                        (xpOption xpLinesOrComments)
                        (xpStatementsArg "intf-spec-lst")
                        (xpOption xpLinesOrComments)
                        (xpElem "end-intf-stmt" $ xpStatement))
                       ,
                       xpElem "internal-subprogram-part" $ -- 22
                       xpWrap (\(obj) -> InternalSubProgramPart obj,
                               \(InternalSubProgramPart obj) -> (obj)) $
                       xpElem "contains-stmt" $
                       xpStatement
                     ,
                       xpWrap (\(obj) -> InternalSubPrograms obj, -- 23
                               \(InternalSubPrograms obj) -> (obj)) $
                       xpProgramUnitsArg "internal-subprogram-lst"
                     ,
                       xpWrap (\(lc) -> StatementLineOrComment lc, -- 24
                               \(StatementLineOrComment lc) -> (lc)) $
                       xpLineOrComment
                     ]

instance XmlPickler Statement where                 
  xpickle = xpStatement

data ExecutableProgram =
   ExecutableProgram {
      epIntrinsicTypes :: TypeDefs,
      epIntrinsicOps :: OpDefs,
      epIntrinsicAssignments :: AssignmentDefs,
      epProgramUnits :: ProgramUnits
   } deriving (Show)

xpExecutableProgram :: PU ExecutableProgram
xpExecutableProgram = xpElem "executable-program" $
                      xpWrap (uncurry4 ExecutableProgram,
                              \ep -> (epIntrinsicTypes ep,
                                      epIntrinsicOps ep,
                                      epIntrinsicAssignments ep,
                                      epProgramUnits ep)) $
                      (xp4Tuple 
                       xpTypeDefs
                       xpOpDefs
                       xpAssignmentDefs
                       (xpProgramUnitsArg "program-unit-lst")
                      )

data File = File { fileName :: String,
                   fileWidth :: Int,
                   fileForm ::  String,
                   fileExecutableProgram :: ExecutableProgram
                 } deriving (Show)

data Fortran95 = Fortran95 { fortran95Namespace :: String,
                             fortran95GlobalSymbolTable :: GlobalSymbolTable,
                             fortran95File :: File
                           } deriving (Show)
                 
xpFile :: PU File
xpFile = xpElem "file" $ 
         xpWrap (\ (name,width,form,line,executableProgram,unseen) -> 
                  File name width form executableProgram
                  ,
                 \s -> (fileName s,
                        fileWidth s,
                        fileForm s,
                        Nothing,
                        fileExecutableProgram s,
                        ()
                        )) $
         (xp6Tuple
          (xpAttr "name" xpText)
          (xpAttr "width" xpPrim)
          (xpAttr "form" xpText)
          (xpOption (xpElem "L" (xpUnit)))
          xpExecutableProgram
          (xpElem "unseen" xpUnit)
          )


instance XmlPickler File where                 
  xpickle = xpFile

xpFortran95 :: PU Fortran95
xpFortran95 = xpElem "fortran95" $
              xpWrap (\ (ns,symtab,file) -> 
                       Fortran95 ns symtab file
                     ,
                      \s -> (fortran95Namespace s,
                             fortran95GlobalSymbolTable s,
                             fortran95File s
                            )) $
              xpTriple
              (xpAttr "xmlns" xpText) 
              xpGlobalSymbolTable
              xpFile
            
instance XmlPickler Fortran95 where                 
  xpickle = xpFortran95

data ArraySymbolInfo = ArraySymbolInfo { arraySymbolInfoArraySpec :: Id }
                     deriving (Eq, Ord, Show)
                     
data SubrSymbolInfo = SubrSymbolInfo { subrSymbolInfoGlobalName :: Id }
                    deriving (Eq, Ord, Show)
                                      
data GSRefInfo =  GSRefInfo { gsRefLocalName :: String,
                              gsRefGlobalName :: String
                            } deriving (Eq, Ord, Show)

data SymbolInfo = None
                | SIArraySymbolInfo ArraySymbolInfo
                | SISubrSymbolInfo SubrSymbolInfo
                | SIGSRefInfo GSRefInfo
                  deriving (Eq, Ord, Show)

data SymbolFlavor = Variable
                    | Subroutine
                    | Module
                    | Parameter deriving (Eq, Ord, Show)

data Symbol = Symbol { symbolId :: Id,
                       symbolDefined :: Bool,
                       symbolFlavor :: SymbolFlavor,
                       symbolGlobalFlag :: Bool,
                       symbolType :: Maybe Id, -- globals have no type
                       symbolInfo :: SymbolInfo
                     } deriving (Eq, Ord, Show)

data AttrSpecsSymbol = AttrSpecsSymbol [String] deriving (Show)

xpAttrSpecsSymbol :: PU AttrSpecsSymbol
xpAttrSpecsSymbol = xpElem "attr-spec-lst" $ 
                    xpWrap (\(obj) -> AttrSpecsSymbol obj,
                            \(AttrSpecsSymbol obj) -> (obj)) $
                    xpList $
                    xpElem "str" $
                    (xpAttr "val" xpText)

data SymbolExtra = SymbolExtra Symbol 
                   (Maybe AttrSpecsSymbol)
                   (Maybe DummyArgsSymbol) deriving (Show)

data SymbolTable = SymbolTable [SymbolExtra] deriving (Show)

data GlobalSymbolTable = GlobalSymbolTable [Symbol] deriving (Show)

castToArrayId :: SymbolInfo -> Maybe Id
castToArrayId obj =
  case obj of
    (SIArraySymbolInfo val) -> Just $ arraySymbolInfoArraySpec val
    _ -> Nothing

castToGlobalName :: SymbolInfo -> Maybe Id
castToGlobalName obj =
  case obj of
    (SISubrSymbolInfo val) -> Just $ subrSymbolInfoGlobalName val
    _ -> Nothing

toBoolean str = if (str == "1") then True else False

fromBoolean flag = if flag then "1" else "0"

toSymbolFlavor symbolFlavorStr = 
  case symbolFlavorStr of
       "VARIABLE" -> Variable
       "SUBROUTINE" -> Subroutine
       "MODULE" -> Module
       "PARAMETER" -> Parameter
       
fromSymbolFlavor symbolFlavor =
  case symbolFlavor of
    Variable -> "VARIABLE"
    Subroutine -> "SUBROUTINE"
    Module -> "MODULE"
    Parameter -> "PARAMETER"

encodeSymbol globalFlag idStr = 
  xpWrap (\ (symbolId,
             symbolFlavorStr,
             symbolDefined,
             symbolType,
             (arraySpec,globalName)
            )
          -> 
           (Symbol
            symbolId 
            (toBoolean symbolDefined)
            (toSymbolFlavor symbolFlavorStr)
            globalFlag
            symbolType
            (if symbolFlavorStr == "VARIABLE" && (isJust arraySpec) then
               SIArraySymbolInfo $ ArraySymbolInfo (fromJust arraySpec)
             else
               if symbolFlavorStr == "SUBROUTINE" && (isJust globalName) then
                 SISubrSymbolInfo $ SubrSymbolInfo (fromJust globalName)
               else
                 None
            ))
            ,
          \s -> (symbolId s,
                 (fromSymbolFlavor $ symbolFlavor s),
                 (fromBoolean $ symbolDefined s),
                 symbolType s,
                 (castToArrayId $ symbolInfo s,
                  castToGlobalName $ symbolInfo s)
                )
            ) $
   (xp5Tuple
    (xpAttr idStr xpText)
    (xpAttr "flavor" xpText)
    (xpAttr "defined" xpText)
    (xpOption (xpAttr "T" xpText)) -- no types for globals
    (xpPair 
     (xpOption (xpAttr "array-spec" xpText))
     (xpOption (xpAttr "G-N" xpText))))

xpSymbolExtra :: PU SymbolExtra
xpSymbolExtra = xpElem "S" $
                xpWrap (\(sym,attr,dummy) -> SymbolExtra sym attr dummy,
                        \(SymbolExtra sym attr dummy) -> (sym,attr,dummy)) $
                (xpTriple (encodeSymbol False "id")
                 (xpOption xpAttrSpecsSymbol)
                 (xpOption xpDummyArgsSymbol))
  
xpLocalSymbolTable :: PU SymbolTable   
xpLocalSymbolTable = xpElem "S-lst" $
                     xpWrap (\(l) -> SymbolTable l,
                             \(SymbolTable l) -> (l)) $
                     xpList $
                     xpSymbolExtra
  
xpGlobalSymbolTable :: PU GlobalSymbolTable   
xpGlobalSymbolTable = xpElem "G-S-lst" $
                      xpWrap (\(l) -> GlobalSymbolTable l,
                              \(GlobalSymbolTable l) -> (l)) $
                      xpList $
                      xpElem "G-S" $
                      encodeSymbol True "N"
                
data Assignment = Assignment { assignmentRefId :: Id,
                               assignmentDefId :: Id
                             }
                deriving (Show)

xpAssignment :: PU Assignment
xpAssignment = xpElem "assgt" $
               xpElem "a" $
               xpWrap (\(ref,def,char) -> Assignment ref def,
                       \(Assignment ref def) -> (ref,def,"=")) $
               (xpTriple
                (xpAttr "id" xpText)
                (xpAttr "S" xpText)
                (xpElem "c" xpText))

data Access = Public deriving (Eq,Ord,Show)

data Generic = Generic {
  genericName :: String,
  genericAccess :: Maybe Access
  }
               
fromAccess sym = case sym of
  Just Public -> Just "PUBLIC"
  Nothing -> Nothing
    
toAccess str = case str of
  Just "PUBLIC" -> Just Public
  Nothing -> Nothing

xpGeneric :: PU Generic
xpGeneric = xpElem "generic" $
            xpWrap (\(name,accessStr) -> Generic name (toAccess accessStr),
                    \(Generic name access) -> (name,(fromAccess access))) $
            (xpPair 
             (xpAttr "N" xpText)
             (xpOption (xpAttr "access" xpText)))

data Generics = Generics [Generic]

xpGenerics :: PU Generics
xpGenerics = xpElem "generic-lst" $
             xpWrap (\(l) -> Generics l,
                     \(Generics l) -> (l)) $
             xpList $
             xpGeneric
             
main        :: IO ()
main
    = do
      ret <-
        (let top = xpFortran95
         in
          do runX ( 
               -- constA (Test 1 2)
               xunpickleDocument top
               [(a_remove_whitespace, v_1),
                (a_validate, v_1)] "update-state.xml"
               --- >>> xpickleDocument top [] "foo.xml"
               ))
      putStrLn $ show ret
      return () 
      

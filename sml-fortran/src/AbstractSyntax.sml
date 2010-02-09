(***********************************************************************
 * Abstract Syntax definition for OFP SML implementation.
 * 
 * Matthew Sottile (matt@galois.com)
 ***********************************************************************)
structure AbstractSyntax =
struct

    structure TK = Tokens;

    (* primitive datatypes *)
    datatype PrimType = INTEGER
		      | REAL
		      | DOUBLEPRECISION
		      | DOUBLECOMPLEX
		      | COMPLEX
		      | LOGICAL
		      | CHARACTER;

    (* program units *)
    datatype UnitType = MODULE
		      | PROGRAM
		      | SUBROUTINE
		      | FUNCTION
		      | INTERFACE;

    (* intents *)
    datatype IntentSpec = INTENT_IN 
			| INTENT_INOUT 
			| INTENT_OUT;

    (* operators for expressions *)

    (* binary operators *)
    datatype BinOp = PLUS 
		   | MINUS 
		   | TIMES 
		   | DIVIDE 
		   | POWER
		   | LOGIC_AND
		   | LOGIC_OR
		   | LOGIC_EQV
		   | LOGIC_NEQV;

    (* unary operators *)
    datatype UnOp = UMINUS
		  | UPLUS
		  | NOT;

    (* relational operators *)
    datatype RelOp = EQUALS 
		   | NOTEQUALS 
		   | LESSTHAN 
		   | LESSEQUAL
		   | GREATERTHAN
		   | GREATEREQUAL;

    (* AST nodes *)
    datatype AbsynNode = 
             INTCONST of {value: int, kind: KindSpec option}
		   | REALCONST of {value: string, kind: KindSpec option}
           | LOGICALCONST of {value: bool, kind: KindSpec option}
		   | STRINGCONST of string
		   | CHARCONST of char
		   | VARIABLE of AbsynNode
		   | BINARYOP of {left: AbsynNode,
				          operand: BinOp,
				          right: AbsynNode}
		   | UNARYOP of {node: AbsynNode,
				         operand: UnOp}
		   | CALLSTMT of AbsynNode
		   | RELOP of {left: AbsynNode,
				       operand: RelOp,
				       right: AbsynNode}
		   | ASSIGN of {lhs: AbsynNode,
				        rhs: AbsynNode}
		   | IMPLICIT_STMT of ImplicitSpec
		   | IFTHENELSE of {cond: AbsynNode,
					        then_clause: BasicBlock option,
					        else_clause: BasicBlock option,
					        elseif: bool,
					        closed: bool}
		   | CYCLE of {name: string option}
		   | WHERE of {cond: AbsynNode,
				       body: BasicBlock,
				       else_clause: BasicBlock option,
				       bclosed: bool,
				       eclosed: bool}
		   | DOLOOP of {ctrl: LoopControl option,
				        body: BasicBlock,
				        label: string option,
				        closed: bool}
           | LABEL of {name: string,
                       child: AbsynNode}
		   (* NOTE: PRINT allows invalid nodes to appear in the
              output item list.  Must check later that the list
              elements are valid in a second pass. *)
		   | PRINT of AbsynNode list
           | DECLARATION of {ty: FType,
					         attributes: Attr list,
                             entities: Entity list}
		   | ARRAYSPEC of ArraySpecElement list
           | SUBSCRIPTLIST of {lo: AbsynNode option, 
                               hi: AbsynNode option, 
                               stride: AbsynNode option} list
           | ACTUALARGLIST of ActualArgSpec list
		   | FUNCSTMT of {name: string,
                          attrs: ProcAttr list,
				          args: string list,
				          body: BasicBlock,
				          scope: FType SymbolTable.table}
		   | FUNCREF of {designator: PartRef list,
                         rtn_type: FType,
				         args: ActualArgSpec list}
           | SUBROUTINE of {name: string,
                            attrs: ProcAttr list,
                            args: string list,
                            body: BasicBlock,
					        scope: FType SymbolTable.table}
           | MODULE of {name: string,
                        body: BasicBlock,
                        procs: AbsynNode list, (* TODO - how to specialize *)
                        scope: FType SymbolTable.table}
		   | PROGSTMT of {name:  string option, 
				          body:  BasicBlock,
				          scope: FType SymbolTable.table }
		   | DATAREF of PartRef list
		   | EXECCONS of AbsynNode
		   | BBLOCK of BasicBlock
	     and ImplicitSpec = 
             IMPLICIT_NONE
		   | IMPLICIT_SPEC of {ty: FType,
					           letters: {id1: string,
							             id2: string option} list} list
         and ActualArgSpec = ACTUALARG of {arg: AbsynNode,
                                           keyword: AbsynNode option}
	     and ArraySpecElement = AS_EXPR of AbsynNode
			      | AS_EXPR_COLON of AbsynNode
			      | AS_EXPR_COLON_EXPR of AbsynNode * AbsynNode
			      | AS_EXPR_COLON_ASTERISK of AbsynNode
			      | AS_ASTERISK
			      | AS_COLON
	     and Entity = ENTITY of {name: string, 
				 (* ARRAYSPEC ASSUMED TO BE ABSYN.ARRAYSPEC *)
				                 arrayspec : AbsynNode option,
				                 initializer : AbsynNode option}
	     and LoopControl = WHILE of AbsynNode
			             | ITERATE of {var: AbsynNode,
				                       lo: AbsynNode,
				                       hi: AbsynNode,
				                       inc: AbsynNode option}
	     and BasicBlock = BLOCK of AbsynNode list
	     and PartRef = PARTREF of {token: ActionParse.token_type,
				                   selectionSubscriptList: AbsynNode option,
				                   imageSelector: AbsynNode option}
	     and KindSpec = LITERAL of int 
		              | IDENT of string
		              | KINDEXPR of AbsynNode
	     and FType = PRIMITIVE of PrimType * KindSpec option
		           | RECORD of {name: string, ty : FType} list
		           | FUNCTYPE of FType * {name: string, 
					                      intent : IntentSpec,
					                      ty : FType} list
		           | PROGUNIT of UnitType
                                 
         (* this is experimental *)
         and ProcAttr = PROC_TYPESPEC of FType
                      | PROC_RECURSIVE
	                  | PROC_PURE
                      | PROC_ELEMENTAL
                      | RESULT_NAME of string
                      | BIND_NAME of string option
                                     
	 (* Attr based on definition of attr-spec in R503 (p71) of the
            Oct 2003 F2003 committee draft *)
	 and Attr = ATTR_ALLOCATABLE
		  | ATTR_ASYNCHRONOUS
		  | ATTR_DIMENSION of ArraySpecElement list
		  | ATTR_EXTERNAL 
		  | ATTR_INTENT of IntentSpec
		  | ATTR_INTRINSIC
		  | ATTR_OPTIONAL
		  | ATTR_PARAMETER
		  | ATTR_POINTER
		  | ATTR_PROTECTED
		  | ATTR_SAVE
		  | ATTR_TARGET
		  | ATTR_VALUE
		  | ATTR_VOLATILE;

    exception BadEnumException;
    exception BadKeyValueException of string;

    (* we use the actionparse structure, so rename it for conciseness *)
    structure AP = ActionParse;

    (* given a list of key/value pairs, find the value associated with the
       provided key.  If none exist, return NONE. *)
    fun get_key_value key [] = NONE
      | get_key_value key (kv::kvs) =
	let val (somekey,somevalue) = kv
	in
	    if (String.compare(key,somekey) = EQUAL) then SOME(somevalue)
	    else (get_key_value key kvs)
	end;

    (* below are the SML equivalents of the enums from the Java code *)
    datatype ARRAY_SPEC = ASPEC_EXPR
			| ASPEC_EXPR_COLON
			| ASPEC_EXPR_COLON_EXPR
			| ASPEC_EXPR_COLON_ASTERISK
			| ASPEC_ASTERISK
			| ASPEC_COLON;

    datatype DECL_TYPESPEC = DECL_INTRINSIC
			   | DECL_TYPE
			   | DECL_CLASS
			   | DECL_UNLIMITED;

    datatype ATTR_SPEC = ATTRSPEC_NONE
		       | ATTRSPEC_ACCESS
		       | ATTRSPEC_LANGUAGE_BINDING
		       | ATTRSPEC_PUBLIC
		       | ATTRSPEC_PRIVATE
		       | ATTRSPEC_ALLOCATABLE
		       | ATTRSPEC_ASYNC
		       | ATTRSPEC_DIMENSION
		       | ATTRSPEC_EXTERNAL
		       | ATTRSPEC_INTENT
		       | ATTRSPEC_INTRINSIC
		       | ATTRSPEC_BINDC
		       | ATTRSPEC_OPTIONAL
		       | ATTRSPEC_PARAMETER
		       | ATTRSPEC_POINTER
		       | ATTRSPEC_PROTECTED
		       | ATTRSPEC_SAVE
		       | ATTRSPEC_TARGET
		       | ATTRSPEC_VALUE
		       | ATTRSPEC_VOLATILE
		       | ATTRSPEC_PASS
		       | ATTRSPEC_NOPASS
		       | ATTRSPEC_NON_OVERRIDABLE
		       | ATTRSPEC_DEFERRED;

    (* translators from the Java enum integer representation to the
       equivalent SML datatype *)

    fun enumToDeclTypeSpec i =
	case i of
	    500 => DECL_INTRINSIC
	  | 501 => DECL_TYPE
	  | 502 => DECL_CLASS
	  | 503 => DECL_UNLIMITED
	  | _ => raise BadEnumException;

    fun enumToIntentType i =
	case i of
	    600 => INTENT_IN
	  | 601 => INTENT_OUT
	  | 602 => INTENT_INOUT
	  | _ => raise BadEnumException;

    fun enumToAttrSpec i =
	case i of
	    800 => ATTRSPEC_NONE
	  | 801 => ATTRSPEC_ACCESS
	  | 802 => ATTRSPEC_LANGUAGE_BINDING
	  | 803 => ATTRSPEC_PUBLIC
	  | 804 => ATTRSPEC_PRIVATE
	  | 805 => ATTRSPEC_ALLOCATABLE
	  | 806 => ATTRSPEC_ASYNC
	  | 807 => ATTRSPEC_DIMENSION
	  | 808 => ATTRSPEC_EXTERNAL
	  | 809 => ATTRSPEC_INTENT
	  | 810 => ATTRSPEC_INTRINSIC
	  | 811 => ATTRSPEC_BINDC
	  | 812 => ATTRSPEC_OPTIONAL
	  | 813 => ATTRSPEC_PARAMETER
	  | 814 => ATTRSPEC_POINTER
	  | 815 => ATTRSPEC_PROTECTED
	  | 816 => ATTRSPEC_SAVE
	  | 817 => ATTRSPEC_TARGET
	  | 818 => ATTRSPEC_VALUE
	  | 819 => ATTRSPEC_VOLATILE
	  | 820 => ATTRSPEC_PASS
	  | 821 => ATTRSPEC_NOPASS
	  | 822 => ATTRSPEC_NON_OVERRIDABLE
	  | 823 => ATTRSPEC_DEFERRED
	  | _ => raise BadEnumException;

    fun enumToArraySpec i =
	case i of
	    700 => ASPEC_EXPR
	  | 701 => ASPEC_EXPR_COLON
	  | 702 => ASPEC_EXPR_COLON_EXPR
	  | 703 => ASPEC_EXPR_COLON_ASTERISK
	  | 704 => ASPEC_ASTERISK
	  | 705 => ASPEC_COLON
	  | _ => raise BadEnumException;

    fun enumToPrimitive i =
	case i of
	    400 => INTEGER
	  | 401 => REAL
	  | 402 => DOUBLEPRECISION
	  | 403 => DOUBLECOMPLEX
	  | 404 => COMPLEX
	  | 405 => CHARACTER
	  | 406 => LOGICAL
	  | _ => raise BadEnumException;

    (* helper functions to translate key/value pairs directly to SML
       datatypes *)
    fun primitiveTypeFromAction kvs =
        let val tnum = get_key_value "type" kvs
        in
            case tnum of
                NONE => raise BadKeyValueException("Missing type key")
              | SOME(v) => (case v of
                                AP.INT(i) => (enumToPrimitive i)
                              | _ => raise BadKeyValueException("Bogus type"))
        end;

    fun intentSpecFromAction kvs =
	let val tnum = get_key_value "intent" kvs
	in
	    case tnum of
		NONE => raise BadKeyValueException("Missing intent key")
	      | SOME(v) => (case v of
				AP.INT(i) => (enumToIntentType i)
			      | _ => raise BadKeyValueException("Bogus type"))
	end;

    fun attrSpecFromAction kvs =
	let val tnum = get_key_value "attr" kvs
	in
	    case tnum of
		NONE => raise BadKeyValueException("Missing type key")
	      | SOME(v) => (case v of
				AP.INT(i) => (enumToAttrSpec i)
			      | _ => raise BadKeyValueException("Bogus type"))
	end;

    fun arraySpecTypeFromAction kvs =
	let val tnum = get_key_value "type" kvs
	in
	    case tnum of
		NONE => raise BadKeyValueException("Missing type key")
	      | SOME(v) => (case v of
				AP.INT(i) => (enumToArraySpec i)
			      | _ => raise BadKeyValueException("Bogus type"))
	end;

    fun declTypeSpecFromAction kvs =
	let val tnum = get_key_value "type" kvs
	in
	    case tnum of
		NONE => raise BadKeyValueException("Missing type key")
	      | SOME(v) => (case v of
				AP.INT(i) => (enumToDeclTypeSpec i)
			      | _ => raise BadKeyValueException("Bogus type"))
	end;

end;

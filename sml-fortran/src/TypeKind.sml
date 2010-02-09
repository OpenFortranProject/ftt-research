(* 
 * Module gathering kind and type information.
 *
 *
 *   Kind values are target compiler and architecture specific
 *   (not sure how to handle this at moment)
 *   Current default_kind values are for a typical 32 bit machine (still compiler dependent)
 *
 * crasmussen@lanl.gov
 *)

signature TYPEKIND =
sig
    val default_kind  : AbstractSyntax.PrimType -> int;
    val compatible    : AbstractSyntax.FType * AbstractSyntax.FType -> bool;
    val prim_type_str : AbstractSyntax.PrimType -> string;
    val resolve       : AbstractSyntax.AbsynNode -> AbstractSyntax.FType SymbolTable.table
                            -> AbstractSyntax.FType option;
    val toString      : AbstractSyntax.FType -> string;
end;

structure TypeKind :> TYPEKIND =

struct
    structure AS = AbstractSyntax;

    exception TODOException;

    (* WARNING ********************************************************************
     * Default kind values are target compiler and architecture dependent
     * TODO - fix this to bring in compiler info
     *)
    fun default_kind(AS.LOGICAL) = 4
      | default_kind(AS.INTEGER) = 4
      | default_kind(AS.REAL) = 4
      | default_kind(AS.DOUBLEPRECISION) = 4
      | default_kind(AS.COMPLEX) = 8
      | default_kind(AS.DOUBLECOMPLEX) = 16
      | default_kind(AS.CHARACTER) = 4
    (* END WARNING ****************************************************************)

    fun kind_value(AS.LITERAL(i))     = i
      | kind_value(AS.IDENT(id))      = raise TODOException
      | kind_value(AS.KINDEXPR(expr)) = raise TODOException

    fun ko_value(t,NONE) = default_kind(t)
      | ko_value(t,SOME(k)) = kind_value(k)

    fun op_kind_str(SOME(k)) = "(" ^ Int.toString( kind_value(k) ) ^ ")"
      | op_kind_str(NONE) = ""

    fun prim_type_str t =
	case t
          of AS.LOGICAL         => "LOGICAL"
           | AS.INTEGER         => "INTEGER"
           | AS.REAL            => "REAL"
           | AS.DOUBLEPRECISION => "DOUBLE PRECISION"
           | AS.COMPLEX         => "COMPLEX"
           | AS.DOUBLECOMPLEX   => "DOUBLE COMPLEX"
           | AS.CHARACTER       => "CHARACTER";
    (* end fun prim_type_str *)

    fun compatible_kinds(pty, k1, k2) = (ko_value(pty,k1)=ko_value(pty,k2))

    fun compatible_primitives((t1,k1), (t2,k2)) =
        if (t1=t2) then (* types are equivalent, check kinds *)
            compatible_kinds(t1,k1,k2)
        else false
    (* end fun compatible_primitives *)

    fun compatible(AS.PRIMITIVE(p1), AS.PRIMITIVE(p2)) = compatible_primitives(p1,p2)
      | compatible(AS.RECORD(r1), AS.RECORD(r2))       = raise TODOException
      | compatible(AS.FUNCTYPE(f1), AS.FUNCTYPE(f2))   = raise TODOException
      | compatible(AS.PROGUNIT(u1), AS.PROGUNIT(u2))   = raise TODOException
      | compatible(ty1,ty2) = raise TODOException (* other combos e.g., prim and functype *)

    fun resolve node symtab =
	case node
	  of AS.INTCONST({value,kind})  => SOME(AS.PRIMITIVE(AS.INTEGER, kind))
           | AS.REALCONST({value,kind}) => SOME(AS.PRIMITIVE(AS.REAL,    kind))
           | AS.VARIABLE(var)           => resolve var symtab
           | AS.BINARYOP({left,operand,right}) =>
             (
               let fun compat(SOME(ty1), SOME(ty2)) = compatible(ty1,ty2)
                     | compat(ty1,ty2) = false
                   val lty = resolve left symtab
                   val rty = resolve right symtab
               in
                   if compat(lty,rty) then lty
                   else
                   (
                       TextIO.print("Incompatible BinOp params\n");
                       NONE
                   )
               end
             )
           | AS.DATAREF(prl:AS.PartRef list) =>
             (
               let
                 fun respr ((AS.PARTREF(s)::[]):AS.PartRef list) =
                     let val part_name = (#id (#token s));
                         val sym = SymbolTable.symbol part_name;
                     in 
                         SymbolTable.look(symtab, sym)
                     end
                   | respr ((AS.PARTREF(s)::ss):AS.PartRef list) =
                         raise TODOException (* how to handle string of partrefs? *)
                   | respr [] =
                         raise TODOException (* check to see if will actually occur *)
              in
                  respr prl
              end
             )
           | _ => NONE  (* TODO - more options? *)
    (* end fun resolve *)

    fun toString(AS.PRIMITIVE(ty,kd)) = prim_type_str(ty) ^ op_kind_str(kd)
      | toString(AS.RECORD(r))        = raise TODOException
      | toString(AS.FUNCTYPE(r))      = raise TODOException
      | toString(AS.PROGUNIT(r))      = raise TODOException
    (* end fun toString *)

end (* structure Type *)

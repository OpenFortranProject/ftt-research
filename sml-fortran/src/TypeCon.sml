(* 
 * Visitor to check for implicit type conversions
 *
 * crasmussen@lanl.gov
 *)

signature TYPECON =
sig

    type Env = {scope: AbstractSyntax.FType SymbolTable.table};

    val check : AbstractSyntax.AbsynNode * Env -> bool;

end;

structure TypeCon :> TYPECON =

struct
    structure AS = AbstractSyntax;
    structure TK = TypeKind;

    type Env = {scope: AbstractSyntax.FType SymbolTable.table}

    fun say s env =
    (
        TextIO.print(s);
        env
    )

    fun sayln s env =
    (
        TextIO.print(s ^ "\n");
        env
    )

    fun check (node, env:Env) =
        case node 
          of AS.ASSIGN({lhs, rhs}) =>
             (
               let
                 val opvar = TK.resolve lhs (#scope env)
                 val opexpr = TK.resolve rhs (#scope env)
                 val junk = case opvar of SOME(vty) =>
                                (case opexpr of SOME(ety) =>
                                    if TK.compatible(vty,ety) then
                                        sayln (TK.toString(vty)^" = "^TK.toString(ety)) env
                                    else
                                        sayln "Incompatible assignment params" env
                                   | _ => sayln "EXPR TYPE_NOT_FOUND" env)
                               | _ => sayln "VAR TYPE_NOT_FOUND" env
               in
                 false
               end
             )
	  | _ => true
    (* end fun check *)

end

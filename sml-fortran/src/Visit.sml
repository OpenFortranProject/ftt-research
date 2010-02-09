(* 
 * Visitor module for AST nodes
 *
 * crasmussen@lanl.gov
 *)

signature VISIT =
sig

    type Env = {scope: AbstractSyntax.FType SymbolTable.table};

    val visit : (AbstractSyntax.AbsynNode * TypeCon.Env -> bool)
                    -> AbstractSyntax.AbsynNode -> Env -> Env;

    val create_env : AbstractSyntax.FType SymbolTable.table -> Env;

end;

structure Visit :> VISIT =

struct
    structure AS = AbstractSyntax;
    structure TK = TypeKind;

    type Env = {scope: AbstractSyntax.FType SymbolTable.table}

    exception TODOException;

    fun create_env scope = {scope=scope}

    fun visit vfunc node env =
        let

        fun visitBasicBlock [] env = env
          | visitBasicBlock (h::rest) env =
            ( 
                visitBasicBlock rest (visit vfunc h env)
            )

        fun visitBlockList [] env = env
          | visitBlockList (h::rest) env =
            (
                visitBlockList rest (visit vfunc h env)
            )

        in (* visit body *)

	case node

	  of AS.INTCONST(i)     => ( vfunc(AS.INTCONST(i), env)  ; env )

           | AS.REALCONST(r)    => ( vfunc(AS.REALCONST(r), env) ; env )

           | AS.LOGICALCONST(l) => ( vfunc(AS.LOGICALCONST(l), env) ; env )

           | AS.STRINGCONST(s)  => ( vfunc(AS.STRINGCONST(s), env) ; env )

           | AS.CHARCONST(c)    => ( vfunc(AS.CHARCONST(c), env) ; env )

           | AS.VARIABLE(v) =>
                 if vfunc(AS.VARIABLE(v), env) then
                     visit vfunc v env
                 else env

           | AS.BINARYOP(bop) =>
                 if vfunc(AS.BINARYOP(bop), env) then
                     let val {left=l, right=r, ...} = bop
                     in visit vfunc r (visit vfunc l env)
                     end
                 else env

           | AS.UNARYOP(uop) =>
                 if vfunc(AS.UNARYOP(uop), env) then
                     let val {node, ...} = uop
                     in visit vfunc node env
                     end
                 else env

           | AS.RELOP(rop) =>
                 if vfunc(AS.RELOP(rop), env) then
                     let val {left=l, right=r, ...} = rop
                     in visit vfunc r (visit vfunc l env)
                     end
                 else env

           | AS.CALLSTMT(node) =>
                 if vfunc(AS.CALLSTMT(node), env) then
                     visit vfunc node env
                 else env

           | AS.ASSIGN(a) =>
                 if vfunc(AS.ASSIGN(a), env) then
                     let val {lhs,rhs} = a
                     in visit vfunc rhs (visit vfunc lhs env)
                     end
                 else env

           | AS.IMPLICIT_STMT(is) => ( vfunc(AS.IMPLICIT_STMT(is), env) ; env )

           | AS.IFTHENELSE(ite) =>
                 if vfunc(AS.IFTHENELSE(ite), env) then
                     let val {cond, then_clause=thenc, else_clause=elsec, ...} = ite
                         val newenv = visit vfunc cond env
                         val newenv = case thenc
                                        of SOME(AS.BLOCK(cl)) => visitBasicBlock cl newenv
                                         | _ => newenv
                         val newenv = case elsec
                                        of SOME(AS.BLOCK(cl)) => visitBasicBlock cl newenv
                                         | _ => newenv
                     in newenv
                     end
                 else env

          | AS.CYCLE(c) => ( vfunc(AS.CYCLE(c), env) ; env )

	  | AS.WHERE(w) =>
                 if vfunc(AS.WHERE(w), env) then
                     let val {cond=mask, body=AS.BLOCK(blk), else_clause=ecl, ...} = w
                         val newenv = visitBlockList blk (visit vfunc mask env)
                     in case ecl
                          of SOME(AS.BLOCK(ew)) => visitBasicBlock ew newenv
                        | _ => newenv
                     end
                 else env

          | AS.DOLOOP(d) =>
                 if vfunc(AS.DOLOOP(d), env) then
                    let val {body=AS.BLOCK(b), ...} = d
                    in visitBasicBlock b env
                    end
                 else env

          | AS.LABEL(l) =>
                 if vfunc(AS.LABEL(l), env) then
                    let val {child, ...} = l
                    in visit vfunc child env
                    end
                 else env

          | AS.PRINT(oil) => ( vfunc(AS.PRINT(oil), env) ; env )

          | AS.DECLARATION(d) => ( vfunc(AS.DECLARATION(d), env) ; env )

          | AS.ARRAYSPEC(asl) => ( vfunc(AS.ARRAYSPEC(asl), env) ; env )

          | AS.SUBSCRIPTLIST(ssl) => ( vfunc(AS.SUBSCRIPTLIST(ssl), env) ; env )

          | AS.ACTUALARGLIST(aal) => ( vfunc(AS.ACTUALARGLIST(aal), env) ; env )

          | AS.DATAREF(prl) => ( vfunc(AS.DATAREF(prl), env) ; env )

          | AS.MODULE(m) =>
              let fun visitProcs [] en = en
                    | visitProcs (h::t) en =
                          let val newenv = visit vfunc h en
                          in visitProcs t newenv
                          end
              in
                  if vfunc(AS.MODULE(m), env) then
                      let val {procs=plist, body=AS.BLOCK(b), scope=symtab, ...} = m
                      in visitProcs plist (visitBasicBlock b {scope=symtab})
                      end
                  else env
              end

          | AS.FUNCREF(f) =>
                if vfunc(AS.FUNCREF(f), env) then
                    let val {args=aalist, ...} = f
                    in visit vfunc (AS.ACTUALARGLIST(aalist)) env
                    end
                else env

          | AS.FUNCSTMT(f) =>
                if vfunc(AS.FUNCSTMT(f), env) then
                    let val {body=AS.BLOCK(b), scope=symtab, ...} = f
                    in visitBasicBlock b {scope=symtab}
                    end
                else env

          | AS.SUBROUTINE(s) =>
                if vfunc(AS.SUBROUTINE(s), env) then
                    let val {body=AS.BLOCK(b), scope=symtab, ...} = s
                    in visitBasicBlock b {scope=symtab}
                    end
                else env

          | AS.PROGSTMT(p) =>
                if vfunc(AS.PROGSTMT(p), env) then
                    let val {body=AS.BLOCK(b), scope=symtab, ...} = p
                    in visitBasicBlock b {scope=symtab}
                    end
                else env
	  | _ => raise TODOException
        end (* visit *)

end

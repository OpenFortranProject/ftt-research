(* 
 * code to unparse ASTs and other structures to Fortran output 
 *
 * crasmussen@lanl.gov
 *)

signature UNPARSE =
sig

    type pInfo = {pfunc:TextIO.vector -> unit, indent:string, level:int, col:int};

    val unparse : AbstractSyntax.AbsynNode -> pInfo -> pInfo;

    val create_pInfo : (TextIO.vector -> unit) -> string -> int -> int -> pInfo;

end;

structure Unparse :> UNPARSE =

struct
    structure AS = AbstractSyntax;
    structure TK = TypeKind;

    exception TODOException;

    type pInfo = {pfunc:TextIO.vector -> unit, indent:string, level:int, col:int}

    fun create_pInfo (pfunc:TextIO.vector -> unit) (ind:string) (l:int) (col:int) =
            {pfunc=pfunc, indent=ind, level=l, col=col}

    fun inc_level (pinfo:pInfo) =
        create_pInfo (#pfunc pinfo) (#indent pinfo) ((#level pinfo) + 1) (#col pinfo)

    fun dec_level (pinfo:pInfo) =
        create_pInfo (#pfunc pinfo) (#indent pinfo) ((#level pinfo) - 1) (#col pinfo)

    fun say s (pinfo:pInfo) =
        let val col = (#col pinfo) + size s
        in
            ( (#pfunc pinfo) s;
              create_pInfo (#pfunc pinfo) (#indent pinfo) (#level pinfo) col
            )
        end;

    fun sayln s (pinfo:pInfo) =
        let val col = (#col pinfo) + size s
        in
            ( (#pfunc pinfo) (s^"\n");
              create_pInfo (#pfunc pinfo) (#indent pinfo) (#level pinfo) 1
            )
        end;

    fun indent (pinfo:pInfo) = 
        let
            fun indent_level 0 (pinfo:pInfo) = (pinfo)
              | indent_level l (pinfo:pInfo) = 
                    let val newpinfo = say (#indent pinfo) pinfo
                    in  indent_level (l-1) newpinfo
                    end
        in
            indent_level (#level pinfo) pinfo
        end;

    (* Strip the '' off from a name *)
    fun strip name = substring(name, 1, size name - 2)

    (* TODO - some of these should be collected into a common location
     *        (i.e., used elsewhere as well *)

    fun present opt = case opt
                        of SOME(n) => 1
                         | NONE    => 0

    fun unparseBinOp binop pinfo =
        case binop
          of AS.PLUS       => say "+"      pinfo
           | AS.MINUS      => say "-"      pinfo
           | AS.TIMES      => say "*"      pinfo
           | AS.DIVIDE     => say "/"      pinfo
           | AS.POWER      => say "**"     pinfo
           | AS.LOGIC_AND  => say ".AND."  pinfo
           | AS.LOGIC_OR   => say ".OR."   pinfo
           | AS.LOGIC_EQV  => say ".EQV."  pinfo
           | AS.LOGIC_NEQV => say ".NEQV." pinfo

    fun unparseUnOp unop pinfo =
        case unop
          of AS.UPLUS   => say "+"      pinfo
           | AS.UMINUS  => say "-"      pinfo
           | AS.NOT     => say ".NOT."  pinfo

    fun unparseRelOp relop pinfo =
        case relop
          of AS.EQUALS        => say "=="  pinfo
           | AS.NOTEQUALS     => say "/="  pinfo
           | AS.LESSTHAN      => say "<"   pinfo
           | AS.LESSEQUAL     => say "<="  pinfo
           | AS.GREATERTHAN   => say ">"   pinfo
           | AS.GREATEREQUAL  => say ">="  pinfo

    fun unparseArgList [] str pinfo = (pinfo)
      | unparseArgList (arg::args) str pinfo =
        (
            unparseArgList args ", " (say (str ^ arg) pinfo)
        )

    fun unparse node pinfo =
        let

        fun unparseOption opt pinfo = case opt
                                        of SOME(node) => unparse node pinfo
                                         | NONE       => pinfo

        fun unparseKind kind pinfo =
            case kind
              of AS.LITERAL(lit)   => say (Int.toString lit) pinfo
               | AS.IDENT(id)      => say id pinfo
               | AS.KINDEXPR(expr) => unparse expr pinfo

        fun unparseOptConstKind kind pinfo =
            case kind
              of SOME(k) => unparseKind k (say "_" pinfo)
               | _       => pinfo

        fun unparseOptKind kind pinfo =
            case kind
              of SOME(k) => say ")" (unparseKind k (say "(" pinfo))
               | _       => pinfo

        fun unparseEntities [] str pinfo = (pinfo)
          | unparseEntities (e::es) str pinfo =
	    let 
  	        val AS.ENTITY({name=name, arrayspec=aspec, initializer=init}) = e
            in
	        let val newpi = unparseOption aspec (say (strip name) (say str pinfo));
                    val newpi = case init
                                  of SOME(expr) => unparse expr (say "=" newpi)
                                   | _          => newpi
                in
                    unparseEntities es ", " newpi
                end
	    end

        fun unparseArraySpecElements [] str pinfo = (pinfo)
          | unparseArraySpecElements (e::es) str pinfo =
            let
                val newpi = say str pinfo;
                val newpi = case e
                              of AS.AS_EXPR(hi) =>
                                     unparse hi newpi
                               | AS.AS_EXPR_COLON(lo) => 
                                     say ":" (unparse lo newpi)
                               | AS.AS_EXPR_COLON_EXPR(lo,hi) =>
                                     unparse hi (say ":" (unparse lo newpi))
                               | AS.AS_EXPR_COLON_ASTERISK(lo) => 
                                     say "*" (say ":" (unparse lo newpi))
                               | AS.AS_ASTERISK => 
                                     say "*" newpi
                               | AS.AS_COLON =>
                                     say ":" newpi
            in
                unparseArraySpecElements es "," newpi
            end

        fun unparseFType ty pinfo =
	    case ty
              of AS.PRIMITIVE(t,k) => unparseOptKind k (say (TK.prim_type_str t) pinfo)
               | _ => say "UNSUPPORTED FTYPE" pinfo

        fun unparsePrefixSpec spec str pinfo =
            case spec
              of AS.PROC_RECURSIVE   => say str (say "RECURSIVE" pinfo)
               | AS.PROC_PURE        => say str (say "PURE" pinfo)
               | AS.PROC_ELEMENTAL   => say str (say "ELEMENTAL" pinfo)
               | AS.PROC_TYPESPEC(f) => say str (unparseFType f pinfo)
               | _ => pinfo

        fun unparsePrefix [] pinfo = pinfo
          | unparsePrefix (s::ss) pinfo =
            (
                unparsePrefix ss (unparsePrefixSpec s " " pinfo)
            )

        fun unparseSuffixSpec spec str pinfo =
            case spec
              of AS.RESULT_NAME(r) => say ("RESULT(" ^ r ^ ")") (say str pinfo)
               | AS.BIND_NAME(b) =>
                 ( case b
                     of SOME(s) => say ("BIND(C,NAME=" ^ s ^ ")") (say str pinfo)
                      | NONE    => say "BIND(C)" (say str pinfo)
                 )
               | _ => pinfo

        fun unparseSuffix [] pinfo = pinfo
          | unparseSuffix (s::ss) pinfo =
            (
                unparseSuffix ss (unparseSuffixSpec s " " pinfo)
            )

        fun unparseAttr attr pinfo =
            case attr
              of AS.ATTR_ALLOCATABLE      => say "ALLOCATABLE"  pinfo
               | AS.ATTR_ASYNCHRONOUS     => say "ASYNCHRONOUS" pinfo
	       | AS.ATTR_EXTERNAL         => say "EXTERNAL"     pinfo
               | AS.ATTR_INTRINSIC        => say "INTRINSIC"    pinfo
               | AS.ATTR_OPTIONAL         => say "OPTIONAL"     pinfo
               | AS.ATTR_PARAMETER        => say "PARAMETER"    pinfo
               | AS.ATTR_POINTER          => say "POINTER"      pinfo
               | AS.ATTR_PROTECTED        => say "PROTECTED"    pinfo
               | AS.ATTR_SAVE             => say "SAVE"         pinfo
               | AS.ATTR_TARGET           => say "TARGET"       pinfo
               | AS.ATTR_VALUE            => say "VALUE"        pinfo
               | AS.ATTR_VOLATILE         => say "VOLATILE"     pinfo
               | AS.ATTR_DIMENSION(aspec) =>
                 (
                   say ")" (unparseArraySpecElements aspec "" (say "DIMENSION(" pinfo))
                 )
               | AS.ATTR_INTENT(ispec)    =>
                 (
                    case ispec
                      of AS.INTENT_IN     => say "INTENT(IN)"    pinfo
                       | AS.INTENT_INOUT  => say "INTENT(INOUT)" pinfo
                       | AS.INTENT_OUT    => say "INTENT(OUT)"   pinfo
                 )

        fun unparseAttributes [] pinfo = (pinfo)
          | unparseAttributes (attr::attrs) pinfo =
            ( 
                unparseAttributes attrs (unparseAttr attr (say ", " pinfo))
            ) 

        fun unparseBasicBlock [] (pinfo:pInfo) = (pinfo)
          | unparseBasicBlock (h::rest) (pinfo:pInfo) =
            ( 
                unparseBasicBlock rest (unparse h pinfo)
            )

        fun unparseBlockList [] pinfo = (pinfo)
          | unparseBlockList (h::rest) pinfo =
            (
                unparseBlockList rest (unparse h pinfo)
            )

        in (* unparse body *)

	case node
	  of AS.INTCONST(i) => unparseOptConstKind (#kind i) (say (Int.toString(#value i)) pinfo)
           | AS.REALCONST(r) => unparseOptConstKind (#kind r) (say (#value r) pinfo)
           | AS.LOGICALCONST(l) =>
                 unparseOptConstKind (#kind l) (say ("." ^ Bool.toString(#value l) ^ ".") pinfo)
           | AS.STRINGCONST(s)  => say s pinfo
           | AS.CHARCONST(c)    => say (str c) pinfo
           | AS.VARIABLE(var)   => unparse var pinfo
           | AS.BINARYOP({left=l,operand=binop,right=r}) =>
             (
               say ")" (unparse r
                       (say " " (unparseBinOp binop (say " " (unparse l (say "(" pinfo))))))
             )
           | AS.UNARYOP({node=n,operand=unop}) =>
             (
               (* TODO - add parens, perhaps not needed *)
               unparse n (unparseUnOp unop pinfo)
             )
           | AS.RELOP({left=l,operand=relop,right=r}) =>
             (
               (* TODO add parens *)
               unparse r (say " " (unparseRelOp relop (say " " (unparse l pinfo))))
             )
           | AS.CALLSTMT(node) =>
             (
               sayln "" (unparse node (say "CALL " (indent pinfo)))
             )
           | AS.ASSIGN({lhs=var, rhs=expr}) =>
             (
               sayln "" (unparse expr (say " = " (unparse var (indent pinfo))))
             )
           | AS.IMPLICIT_STMT(is) =>
             (
               case is
                 of AS.IMPLICIT_NONE => sayln "IMPLICIT NONE" (indent pinfo)
                 | _ => sayln "**UNSUPPORTED**" pinfo (* TODO - finish *)
             )
           | AS.IFTHENELSE({cond=c, then_clause=thenc, else_clause=elsec, closed=cl,...}) =>
             (
               let val newpi = sayln ") THEN" (unparse c (say "IF (" (indent pinfo)));
                   val newpi =
                       case thenc
                         of SOME(AS.BLOCK(cl)) =>
                             dec_level (unparseBasicBlock cl (inc_level newpi))
                          | _ => newpi
                   val newpi =
                       case elsec
                         of SOME(AS.BLOCK(cl)) =>
                             dec_level (unparseBasicBlock cl (inc_level (sayln "ELSE" (indent newpi))))
                          | _ => newpi
               in
                   sayln "END IF" (indent newpi)
               end
             )
          | AS.CYCLE({name=name}) =>
            (
              case name
                of SOME(s) => sayln "CYCLE" (say s (indent pinfo))
                 | _       => sayln "CYCLE" (indent pinfo)
            )
          | AS.DOLOOP(d) =>
            (
              raise TODOException
            )
          | AS.LABEL(l) =>
            (
              raise TODOException
            )
	  | AS.WHERE({cond=mask, body=AS.BLOCK(blk), else_clause=ecl, bclosed=bc, eclosed=ec}) =>
            (
              let val newpi = sayln ")" (unparse mask (say "WHERE (" (indent pinfo)));
                  val newpi = dec_level (unparseBlockList blk (inc_level newpi))
                  val newpi = case ecl
                                of SOME(AS.BLOCK(ew)) =>
                                    let val new_pi = sayln "ELSE WHERE" (indent newpi)
                                    in  dec_level (unparseBasicBlock ew (inc_level new_pi))
                                    end
                                 | _ => newpi
              in
                  sayln "END WHERE" (indent newpi)
              end
            )
          | AS.PRINT(oil) =>
            (
              (* TODO - format *)
              let fun unpoil [] pinfo = pinfo
                    | unpoil (oi::oil) pinfo =
                          unparse oi (say ", " pinfo)
              in
                  sayln "" (dec_level (unpoil oil (inc_level (say "PRINT *" (indent pinfo)))))
              end
            )
          | AS.DECLARATION({ty=ty, attributes=attrs, entities=ents}) =>
            (
              let val newpi = unparseAttributes attrs (unparseFType ty (indent pinfo));
              in
                  sayln "" (unparseEntities ents "" (say " :: " newpi))
              end
            )
          | AS.ARRAYSPEC(elements) =>
            ( 
              say ")" (unparseArraySpecElements elements "" (say "(" pinfo))
            )
          | AS.SUBSCRIPTLIST(ssl) =>
	    (
              let fun unpssl [] str pinfo = pinfo
		    | unpssl ({lo=lo, hi=hi, stride=st}::ss) str pinfo =
		      ( let val unpopt = unparseOption;
		            val ssl = 100*(present lo) + 10*(present hi) + (present st);
                            val newpi = say str pinfo;
                            val newpi = case ssl
                                of 000 => say ":" newpi
                                 | 100 => unpopt lo newpi
                                 | 010 => unpopt hi (say ":" newpi)
                                 | 110 => unpopt hi (say ":" (unpopt lo newpi))
                                 | 001 => unpopt st (say "::" newpi)
                                 | 101 => unpopt st (say "::" (unpopt lo newpi))
                                 | 011 => unpopt st (say ":" (unpopt hi (say ":" newpi)))
                                 | 111 => unpopt st (say ":" (unpopt hi (say ":" (unpopt lo newpi))))
                                 |   _ => newpi
                        in
			   unpssl ss "," newpi
		        end
                      )
	      in
                  if (length ssl > 0) then
                      say ")" (unpssl ssl "" (say "(" pinfo))
                  else
                      pinfo
              end
            )
          | AS.ACTUALARGLIST(aal) =>
            (
              let fun unpaal [] str pinfo = pinfo
                    | unpaal (AS.ACTUALARG(aa)::rest) str pinfo =
                          let val key = case (#keyword aa)
                                          of SOME(AS.STRINGCONST(s)) => s ^ "="
                                           | _ => ""
                          in
                              unpaal rest "," (unparse (#arg aa) (say (str ^ key) pinfo))
                          end
	      in
                  say ")" (unpaal aal "" (say "(" pinfo))
              end
            )
          | AS.DATAREF(prl:AS.PartRef list) =>
            (
              let fun unppr [] pinfo = (pinfo)
                    | unppr ((AS.PARTREF(s)::ss):AS.PartRef list) pinfo =
                          (* TODO - shouldn't part_name be turned into a variable and then unparsed *)
                          let val part_name = strip (#id (#token s));
                              val newpi = unppr ss (say part_name pinfo)
                              val newpi = unparseOption (#imageSelector s) newpi
                          in 
                              unparseOption (#selectionSubscriptList s) newpi
                          end
              in
                  unppr prl pinfo
              end
            )
          | AS.MODULE({name=name, procs=plist, body=AS.BLOCK(b), scope=symtab}) =>
            (
              let fun unparseProcs [] p_info = p_info
                    | unparseProcs (h::t) p_info =
                          let val newpi = unparse h p_info
                              val newpi = sayln "" newpi
                          in
                              unparseProcs t p_info
                          end;
                  val newpi = sayln ("MODULE " ^ name) pinfo;
                  val newpi = dec_level (unparseBasicBlock b (inc_level newpi))
                  val newpi = if (length plist > 0) then
                                  sayln "\nCONTAINS\n" newpi
                              else newpi;
                  val newpi = unparseProcs plist newpi
              in
                  sayln ("END MODULE " ^ name) newpi
              end
            )
          | AS.FUNCSTMT({name=name, attrs=attrs,
                            args=args, body=AS.BLOCK(b), scope=symtab}) =>
            (
              let val newpi = unparsePrefix attrs pinfo;
                  val newpi = say ")" (unparseArgList args ""
                                            (say ("FUNCTION " ^ name ^ "(") newpi));
                  val newpi = sayln "" (unparseSuffix attrs pinfo)
              in
                  sayln ("END FUNCTION " ^ name)
                        (dec_level (unparseBasicBlock b (inc_level newpi)))
              end
            )
          | AS.FUNCREF({designator=prlist, rtn_type=rtn, args=aalist}) =>
            (
              let val newpi = unparse (AS.DATAREF(prlist)) pinfo
              in
                 unparse (AS.ACTUALARGLIST(aalist)) newpi
              end
            )
          | AS.SUBROUTINE({name=name, attrs=attrs,
                              args=args, body=AS.BLOCK(b), scope=symtab}) =>
            (
              let val newpi = unparsePrefix attrs pinfo;
                  val newpi = say ")" (unparseArgList args ""
                                            (say ("SUBROUTINE " ^ name ^ "(") newpi))
                  val newpi = sayln "" (unparseSuffix attrs pinfo)
              in
                  sayln ("END SUBROUTINE " ^ name)
                        (dec_level (unparseBasicBlock b (inc_level newpi)))
              end
            )
          | AS.PROGSTMT({name=name, body=AS.BLOCK(b), scope=symtab}) =>
            (
              let val name = case name
                               of SOME(n) => strip n
                                | _       => ""
                  val newpi = sayln ("PROGRAM " ^ name) pinfo
              in
                  sayln ("END PROGRAM " ^ name)
                        (dec_level (unparseBasicBlock b (inc_level newpi)))
              end
            )
	  | _ => raise TODOException
        end (* unparse *)

end

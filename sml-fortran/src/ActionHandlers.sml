(*
 * Handlers for actions that are emitted by the parser to create
 * abstract syntax.  This is the SML equivalent of the FortranParserAction
 * classes in Java.
 *
 * matt@galois.com
 *)
structure ActionHandlers =
struct
    (* useful structures, renamed for conciseness *)
    structure TK = Tokens;
    structure AP = ActionParse;
    structure ABSYN = AbstractSyntax;
    structure SYMTAB = SymbolTable;

    (* map the symbol table type to a shorter name locally *)
    type symtab = ABSYN.FType SYMTAB.table;

    (* specialized types for building intermediate data structures that 
       aren't part of the proper AST go here. note: letter list technically
       should be chars, not strings. *)
    datatype PARSE_ELEMENT = 
             TYPE of ABSYN.FType
		   | LETTER_LIST of {id1: string,
					         id2: string option} list
		   | ENTITY_DECLS of ABSYN.Entity list
		   | TYPE_DECL of {ty: PARSE_ELEMENT,
                           entities: PARSE_ELEMENT list}
	       | AMB_PARTREF of {token: ActionParse.token_type,
                             selSubOrArgList: SelSubOrArg list,
			                 imageSelector: ABSYN.AbsynNode option}
	       | AMB_DATAREF of {token: ActionParse.token_type,
                             selSubOrArgList: SelSubOrArg list,
			                 imageSelector: ABSYN.AbsynNode option} list
		   | ATTRS of ABSYN.Attr list
		   | PROC_ATTR of ABSYN.ProcAttr
		   | PROCATTRLIST of ABSYN.ProcAttr list
           | PROCLIST of ABSYN.AbsynNode list
		   | OUTPUTITEMS of ABSYN.AbsynNode list
		   | INTENT of ABSYN.IntentSpec
		   | KINDSELECTOR of ABSYN.KindSpec
           | GENERICNAMELIST of string list
           | DUMMYARGLIST of string list
           | SS_ARG_LIST of SelSubOrArg list
		   | INITIALIZATION of ABSYN.AbsynNode
		   | LABEL of ActionParse.token_type
         and SelSubOrArg = SS_ARG of {lo: ABSYN.AbsynNode option,
                                      hi: ABSYN.AbsynNode option, 
                                      stride: ABSYN.AbsynNode option,
                                      arg: ABSYN.ActualArgSpec option};
	     
    (* the context is the state passed into the action handler routines.
       it contains a symbol table (c_scope), a stack of Abstract Syntax
       data structures, and a stack of intermediate data structures. *)
    type context = {c_scope:     symtab,
		    absyn_stack: ABSYN.AbsynNode list,
                    parse_stack: PARSE_ELEMENT list};

    (* exception with a string argument *)
    exception ActionHandlerException of string

    (* print a warning.  warnings are used currently to notify the
       user of actions or handlers that are partially implemented or
       have questionable conformance to the J3 spec. *)
    fun warn s = TextIO.print("WARNING: Not handling "^s^"\n");
	
    fun fatal s = TextIO.print("FATAL ERROR: Not handling ["^s^"]\n");

    (* similar to warn, except it absorbs the string and does nothing *)
    fun ignore s = ();
	
    (* create an empty context *)
    fun empty_context () = {c_scope = SYMTAB.empty,
			    absyn_stack = ([] : ABSYN.AbsynNode list),
                            parse_stack = ([] : PARSE_ELEMENT list) };

    (*********************************************************************)
    (** HELPER FUNCTIONS                                                **)
    (*********************************************************************)
	
    (* given a list of key/value pairs, find the value associated with the
       provided key.  If none exist, return NONE. *)
    fun get_key_value key [] = NONE
      | get_key_value key ((somekey,somevalue)::kvs) =
        if (String.compare(key,somekey) = EQUAL) then 
	        SOME(somevalue)
        else 
            (get_key_value key kvs);
	    
    (* unzip a list of pairs to be a pair of lists *)
    fun unzip [] = ([],[])
      | unzip ((a,b)::rest) = let val (aa,bb) = unzip rest
                              in (a::aa,b::bb) end;
        
    exception HelperException;

    (* zip a pair of lists into a list of pairs *)
    fun zip [] [] = []
      | zip (a::aa) [] = raise HelperException
      | zip [] (b::bb) = raise HelperException
      | zip (a::aa) (b::bb) = ((a,b)::(zip aa bb));
        
    (* STACK MANIPULATION *)
    fun push_absyn_stack anode (ctxt:context) =
        {c_scope=(#c_scope ctxt),
         absyn_stack=( (anode)::(#absyn_stack ctxt) ),
         parse_stack=(#parse_stack ctxt)};
        
    fun pop_absyn_stack ctxt =
        let val {c_scope=scope,
                 absyn_stack=stack,
                 parse_stack=pstack} = ctxt
        in
            case stack of
                [] => ( NONE, ctxt )
              | _ => ( SOME (hd(stack)), {c_scope=scope,
                                          absyn_stack=( tl(stack) ),
                                          parse_stack=pstack} )
        end;

    fun push_parse_stack pnode (ctxt:context) =
        {c_scope=(#c_scope ctxt),
         absyn_stack=(#absyn_stack ctxt),
         parse_stack=( (pnode)::(#parse_stack ctxt) )};
        
    fun pop_parse_stack ctxt =
        let val {c_scope=scope,
                 absyn_stack=stack,
                 parse_stack=pstack} = ctxt
        in
            case pstack of
                [] => ( NONE, ctxt )
              | _ => ( SOME (hd(pstack)), {c_scope=scope,
                                           absyn_stack=stack,
                                           parse_stack=( tl(pstack) )} )
        end;
	
    fun peek_parse_stack (ctxt:context) =
	case (#parse_stack ctxt) of
	    [] => NONE
	  | _ => SOME(hd(#parse_stack ctxt));

    fun peek_absyn_stack (ctxt:context) =
	case (#absyn_stack ctxt) of
	    [] => NONE
	  | _ => SOME(hd(#absyn_stack ctxt));

    (* dequote: take strings that look like 'string' and chop off the
       leading and trailing quote characters.  useful in dealing with
       tokens *)
    fun dequote tok = String.extract(tok,1,SOME ((String.size tok)-2))

    fun extract_kind_spec (tok:AP.token_type) =
        let val ks = dequote (#id tok);
        in
            case (#ty tok) of
                TK.T_IDENT => SOME(ABSYN.IDENT(ks))
              | TK.T_DIGIT_STRING =>
                    let val i = Int.fromString(ks);
                    in
                        case i of
                            SOME(x) => SOME(ABSYN.LITERAL(x))
                          | NONE => raise AP.ActionParseException("Bad Int")
                    end
              | _ => NONE
        end;

    (* TODO - check symbol table for symbol type *)
    fun is_func_ref name [] ctxt = false
      | is_func_ref name salist ctxt =
        let fun has_arg [] prev = prev
              | has_arg (SS_ARG(ssarg)::rest) prev =
                    case (#arg ssarg) of
                        SOME(a) => has_arg rest true
                      | _ => has_arg rest prev
        in
            (* if list has keyword argument it is a function reference *)
            has_arg salist false
        end;

    fun convert_to_ss_list [] new_list = new_list
      | convert_to_ss_list (SS_ARG(ssa)::rest) new_list =
        let val ss = {lo=(#lo ssa), hi=(#hi ssa), stride=(#stride ssa)}
        in
            convert_to_ss_list rest (new_list@[ss])
        end;

    fun convert_to_aa_list [] new_list = new_list
      | convert_to_aa_list (SS_ARG(ssa)::rest) new_list =
        let val arg_op = (#arg ssa)
            val arg = case arg_op of
                          SOME(a) => a
                        | _ => 
                          let val aa =
                                  case (#lo ssa) of
                                      SOME(aa) => aa
                                    | NONE => raise AP.ActionParseException
                                                       ("Bad actual argument")
                          in
                              ABSYN.ACTUALARG({arg=aa, keyword=NONE})
                          end
        in
            convert_to_aa_list rest (new_list@[arg])
        end;

    fun convert_amb_partref apr ctxt =
        let val {token=part_name,
                 selSubOrArgList=salist,
                 imageSelector=is}       = apr
            val isfuncref = is_func_ref part_name salist ctxt
        in
            if isfuncref then
                let val aal = convert_to_aa_list salist []
                    val pr = ABSYN.PARTREF({token=part_name,
                                            selectionSubscriptList=NONE,
                                            imageSelector=NONE})
                in
                    ABSYN.FUNCREF{designator=[pr],
                                  rtn_type=ABSYN.PRIMITIVE(ABSYN.INTEGER,NONE),
                                  args=aal}
                end
            else
                let val ssl = ABSYN.SUBSCRIPTLIST(convert_to_ss_list salist [])
                    val pr = ABSYN.PARTREF{token=part_name,
                                           selectionSubscriptList=SOME(ssl),
                                           imageSelector=(#imageSelector apr)}
                in
                    ABSYN.DATAREF([pr])
                end
        end;

    (* ambigous part-ref is part-name ( ssl ) or proc-name ( aal )
       only last part-ref may be a proc-component-ref *)

    fun convert_ambiguous_dataref [] new_list ctxt = ABSYN.DATAREF(new_list)
      | convert_ambiguous_dataref (apr::[]) new_list ctxt =
            let val {token=part_name,
                     selSubOrArgList=salist,
                     imageSelector=is}       = apr
                val node = convert_amb_partref apr ctxt
            in
                case node of
                    ABSYN.DATAREF(prl) => ABSYN.DATAREF(new_list@prl)
                  | ABSYN.FUNCREF({designator=prl, rtn_type=rtn, args=aal}) =>
                        ABSYN.FUNCREF{designator=(new_list@prl),
                                      rtn_type=rtn,
                                      args=aal}
                  | _ => raise ActionHandlerException("Bad ambiguous DR")
            end
      | convert_ambiguous_dataref (apr::rest) new_list ctxt =
            let val {token=part_name,
                     selSubOrArgList=salist,
                     imageSelector=is}       = apr
                val ssl = ABSYN.SUBSCRIPTLIST(convert_to_ss_list salist [])
                val pr = ABSYN.PARTREF{token=part_name,
                                       selectionSubscriptList=SOME(ssl),
                                       imageSelector=is}
            in
                convert_ambiguous_dataref rest (new_list@[pr]) ctxt
            end;


    (* ================================================== *)
    (* ACTION HANDLERS                                    *)
    (* ================================================== *)

    (* R406 *)
    fun int_literal_constant_handler kvs ctxt =
        let val i = AP.extract_int (get_key_value "digitString" kvs)
            val k = extract_kind_spec
                        (AP.extract_token (get_key_value "kindParam" kvs))
        in
            push_absyn_stack (ABSYN.INTCONST({value=i, kind=k})) ctxt
        end;

    (* R417 *)
    fun real_literal_constant_handler kvs ctxt =
        let val r = AP.extract_real (get_key_value "realConstant" kvs);
            val k = extract_kind_spec
                        (AP.extract_token (get_key_value "kindParam" kvs))
        in
            push_absyn_stack
                (ABSYN.REALCONST({value=r, kind=k})) ctxt
        end;

    (* R 428 *)
    fun logical_literal_constant kvs ctxt =
        let val l = AP.extract_bool (get_key_value "isTrue" kvs)
            val k = extract_kind_spec
                        (AP.extract_token (get_key_value "kindParam" kvs))
        in
            push_absyn_stack (ABSYN.LOGICALCONST({value=l, kind=k})) ctxt
        end;

    fun char_literal_constant kvs ctxt =
	let val s = (#id (AP.extract_token (get_key_value "str" kvs)))
	in
	    push_absyn_stack (ABSYN.STRINGCONST(s)) ctxt
	end;
        
    (* R304 *)
    fun name kvs ctxt =
	let val name = (#id (AP.extract_token (get_key_value "name" kvs)))
        in
	    push_absyn_stack (ABSYN.STRINGCONST(name)) ctxt
        end;

    (* R509 *)
    fun language_binding_spec kvs ctxt =
        let val hasName = AP.extract_bool (get_key_value "hasName" kvs)
            (* ignore the language key_value as it can only be C *)
            val (name,newctxt) = if (hasName) then (pop_absyn_stack ctxt)
                                 else (NONE, ctxt);
            (* the NAME= token needs to be dropped *)
            val (junk,newctxt) = if (hasName) then (pop_absyn_stack newctxt)
                                 else (NONE, newctxt);
        in
            case name of
                SOME(ABSYN.STRINGCONST(s)) =>
                    push_parse_stack (PROC_ATTR
                        (ABSYN.BIND_NAME(SOME(dequote s)))) newctxt
              | _ => push_parse_stack
                        (PROC_ATTR(ABSYN.BIND_NAME(NONE))) newctxt
        end;

    (* R1229 *)
    fun suffix kvs ctxt =
        let val hasBindSpec = AP.extract_bool
                                  (get_key_value "hasProcLangBindSpec" kvs)
            val hasResult =
                ( case (get_key_value "result" kvs) of
                      SOME(anytype) => true
                    | NONE          => false
                );
            val (bspec,newctxt) = if (hasBindSpec) then (pop_parse_stack ctxt)
                                  else (NONE, ctxt);
            val (res,newctxt) = if (hasResult) then (pop_absyn_stack newctxt)
                                else (NONE, newctxt);
            val rlist = 
                ( case res of
                      SOME(ABSYN.STRINGCONST(s)) =>
                          [ABSYN.RESULT_NAME(dequote s)]
                    | _ => []
                );
            val blist = 
                ( case bspec of
                      SOME(PROC_ATTR(b)) => [b]
                    | NONE               => []
                    | _ => raise ActionHandlerException("Bad Parse Stack")
                );
        in
            push_parse_stack (PROCATTRLIST (rlist@blist)) newctxt
        end;

    (* R102 *)
    fun generic_name_list_begin kvs ctxt =
        push_parse_stack (GENERICNAMELIST([])) ctxt;

    fun generic_name_list_part kvs ctxt =
        let val (elt, new_ctxt) = (pop_parse_stack ctxt)
        in
            case elt of
                SOME(GENERICNAMELIST(gnlist)) =>
                let val gnval = AP.extract_token (get_key_value "id" kvs)
                in
                    push_parse_stack
                        (GENERICNAMELIST(gnlist@[dequote (#id gnval)]))
                        new_ctxt
                end
              | _ => raise ActionHandlerException("Bad Parse Stack")
        end;

    (* NOTE: 7/30/07 -- not sure if the below was appropriate, so it
       has been left out for now. *)

    (*        
     fun generic_name_list kvs ctxt =
         let val {c_scope=scope, absyn_stack=stack, parse_stack=pstack} = ctxt
             val (elt, new_ctxt) = (pop_parse_stack ctxt)
             fun gname_in_scope [] s = s
               | gname_in_scope (g::gs) s =
                 let val (new_s, sym) = SYMTAB.insert g s
                 in
                     gname_in_scope gs new_s
                 end
         in
             case elt of
                 SOME(GENERICNAMELIST(gnlist)) =>
                 {c_scope=(gname_in_scope gnlist scope),
                  absyn_stack=stack,
                  parse_stack=(#parse_stack new_ctxt)}
               | _ => raise ActionHandlerException("Bad Parse Stack")
         end;
     *)

    (* replacement: this effectively eats the generic-name-list action and
       does nothing. *)
    fun generic_name_list kvs ctxt =
	ctxt;
        
    (* R1104 *)
    fun module kvs ctxt =
        let fun pop_body body splist context =
                let val (elt,new_ctxt) = (pop_absyn_stack context)
                in
                    case elt of
                        SOME(ABSYN.MODULE(m)) =>
                        ( push_absyn_stack (ABSYN.MODULE({name=(#name m),
                                            procs=splist,
                                            scope=(#c_scope new_ctxt):symtab,
                                            body=ABSYN.BLOCK(body)}))
                          new_ctxt
                        )
                      | SOME(othertype) =>
                            pop_body (othertype::body) splist new_ctxt
                      | _ => raise ActionHandlerException("Bad Module Stmt")
                end
            val hasSPP = true; (* AP.extract_bool (get_key_value "hasSubprogramPart" kvs))); *)
            val (subpp,newctxt) = if (hasSPP) then (pop_parse_stack ctxt)
                                  else (NONE, ctxt);
            val spplist = 
                ( case subpp of
                      SOME(PROCLIST(l)) => l
                    | _ => []
                );
        in
            (pop_body [] spplist newctxt)
        end;

    (* R1105 *)
    fun module_stmt kvs ctxt =
        let val mname = dequote
                          (#id (AP.extract_token (get_key_value "id" kvs)))
	in
            push_absyn_stack (ABSYN.MODULE({name=mname,
                                            procs=[],
                                            scope=(#c_scope ctxt),
                                            body=(ABSYN.BLOCK([]))}))
            ctxt
        end;

    (* R1106 *)
    fun end_module_stmt kvs ctxt =
        (* TODO - handle name=null token properly *)
        let val mname = dequote
                          (#id (AP.extract_token (get_key_value "id" kvs)));
            (* TODO - ERRCHK - make sure module names agree *)
        in
            ctxt
        end;

    (* R1107 *)
    fun module_subprogram_part kvs ctxt =
        (* TODO - need to get count of module_subprograms? *)
        (* grab all subprograms and put in program list PROCLIST*)
        let fun pop_procs 0 lst context = (lst,context)
              | pop_procs n lst context =
                    let val (elt,newctxt) = (pop_absyn_stack context)
                        val proc = case elt of
                                       SOME(ABSYN.SUBROUTINE(s)) =>
                                           ABSYN.SUBROUTINE(s)
                                     | SOME(ABSYN.FUNCSTMT(f)) =>
                                           ABSYN.FUNCSTMT(f)
                                     | _ => raise
                                             ActionHandlerException
                                              ("Bad module_subprogram_part");
                    in
                        pop_procs (n-1) ([proc]@lst) newctxt
                    end
            val count = 2; (* TODO AP.extract_int (get_key_value "count" kvs); *)
            val (plist,newctxt) = pop_procs count [] ctxt;
        in
            push_parse_stack (PROCLIST plist) newctxt
        end;

    (* R1108 *)
    fun module_subprogram kvs ctxt =
        (* TODO - handle prefix *)
        ctxt;

    (* R1224 *)
    fun function_stmt kvs ctxt =
        let val fname = dequote
                          (#id (AP.extract_token (get_key_value "name" kvs)));
            val hasGNList = AP.extract_bool 
                                (get_key_value "hasGenericNameList" kvs);
            val hasSuffix = AP.extract_bool (get_key_value "hasSuffix" kvs);
            val (slist,newctxt) = if (hasSuffix) then (pop_parse_stack ctxt)
                                  else (NONE, ctxt);
            val (gnlist,newctxt) = if (hasGNList) then (pop_parse_stack newctxt)
                                   else (NONE, newctxt);
            val glist = case gnlist of
                            SOME(GENERICNAMELIST(l)) => l
                          | _ => [];
            val attrlist = case slist of
                               SOME(PROCATTRLIST(l)) => l
                             | _ => [];
        in
            (push_absyn_stack (ABSYN.FUNCSTMT({name=fname,
                                               attrs=attrlist,
                                               args=glist,
                                               scope=(#c_scope newctxt),
                                               body=(ABSYN.BLOCK([]))}))
			      newctxt)
        end;

    fun end_function_stmt kvs ctxt =
        (* TODO - handle name=null token properly *)
        let val fname = AP.extract_token (get_key_value "name" kvs)
            fun pop_body context body =
                let val (elt,new_ctxt) = (pop_absyn_stack context)
                (* TODO - ERRCHK - make sure names, if present agree *)
                in
                    case elt of
                        SOME(ABSYN.FUNCSTMT(f)) =>
                        (push_absyn_stack
                             (ABSYN.FUNCSTMT({name=(#name f),
                                              scope=(#c_scope new_ctxt):symtab,
                                              body=ABSYN.BLOCK(body),
                                              args=(#args f),
                                              attrs=(#attrs f)}))
                             new_ctxt)
                      | SOME(othertype) => (pop_body new_ctxt
                                                     (othertype::body))
                      | _ => raise ActionHandlerException("Bad Func. Stmt")
                end
        in
            (pop_body ctxt [])
        end;


    fun ext_function_subprogram kvs ctxt =
	let val hasPref = AP.extract_bool (get_key_value "hasPrefix" kvs);
            val (pref,newctxt) = if (hasPref) then (pop_parse_stack ctxt)
                                 else (NONE, ctxt);
            val (elt,newctxt) = (pop_absyn_stack newctxt);
            val plist = case pref of
                            SOME(PROCATTRLIST(l)) => l
                          | _ => []
        in
            case elt of
                SOME(ABSYN.FUNCSTMT(f)) =>
                  (push_absyn_stack
                     (ABSYN.FUNCSTMT({name=(#name f),
                                      scope=(#c_scope newctxt):symtab,
                                      body=(#body f),
                                      args=(#args f),
                                      attrs=(plist@(#attrs f))}))
                     newctxt)
              | _ => raise ActionHandlerException("Bad Func. Stmt")
        end;

    (* R 613 *)
    fun part_ref kvs ctxt =
        let val part_name = AP.extract_token (get_key_value "id" kvs);
            val hssl = AP.extract_bool (get_key_value
                                            "hasSelectionSubscriptList" kvs);
            val his = AP.extract_bool (get_key_value "hasImageSelector" kvs);
	    val (sslop,newctxt) = if (hssl) then (pop_parse_stack ctxt)
			          else (NONE,ctxt);
            val ssal = case sslop of
                          SOME(SS_ARG_LIST(l)) => l
                        | _ => []
        in
            push_parse_stack (AMB_PARTREF{token=part_name,
                                          selSubOrArgList=ssal,
                                          imageSelector=NONE})
                             newctxt
        end;
        
    (* R 612 *)
    fun data_ref kvs ctxt =
        let val numparts = AP.extract_int (get_key_value "numPartRef" kvs)
            fun get_part_ref_list 0 ctxt = ([],ctxt)
              | get_part_ref_list n ctxt =
                let val (pval,new_ctxt) = pop_parse_stack ctxt
                in
                    case pval of
                        SOME(AMB_PARTREF(pr)) =>
                        let val (rest,newest_ctxt) =
                                get_part_ref_list (n-1) new_ctxt
                        in
                            ( (rest@[pr]), newest_ctxt )
                        end
                      | _ => raise ActionHandlerException("Bad Part Ref in DR")
                end;
            val (aprlist, new_ctxt) = get_part_ref_list numparts ctxt;
            val node = convert_ambiguous_dataref aprlist [] new_ctxt
        in
            push_absyn_stack node new_ctxt
        end;
        
    (* R 404 *)
    fun kind_selector kvs ctxt = 
	let val hasExpression =
		AP.extract_bool (get_key_value "hasExpression" kvs)
	in
	    case hasExpression of
		true => let val (elt,newctxt) = (pop_absyn_stack ctxt)
			in
			    case elt of 
				SOME(ABSYN.INTCONST(i)) =>
				push_parse_stack 
				    (KINDSELECTOR(ABSYN.LITERAL(#value i))) newctxt
			      | SOME(i) => 
				push_parse_stack 
				    (KINDSELECTOR(ABSYN.KINDEXPR(i))) newctxt
			      | NONE => 
				raise ActionHandlerException("bad kind expr")
			end
	      | _ => (warn "CANNOT HANDLE NO EXPRESSION ON KIND";
		      raise ActionHandlerException("unimplemented"))
	end;

    (* R 403 *)
    fun intrinsic_type_spec kvs ctxt =
	let val pt = ABSYN.primitiveTypeFromAction kvs;
	    val hasKind = AP.extract_bool (get_key_value "hasKindSelector" kvs)
	in
	    push_parse_stack 
		(case hasKind of
		     true => 
		       let val (ks,new_ctxt) = 
			       pop_parse_stack ctxt
		       in
			   case ks of
			       SOME(KINDSELECTOR(ksel)) =>
			       TYPE(ABSYN.PRIMITIVE(pt,SOME(ksel)))
			     | _ =>
			       raise ActionHandlerException("Bad Kind Sel.")
		       end
		   | false => TYPE(ABSYN.PRIMITIVE(pt,NONE))
		)
		ctxt
	end;
	
    (* R 506 *)
    fun initialization kvs ctxt =
	let val (elt, newctxt) = (pop_absyn_stack ctxt)
	in
	    case elt of
		SOME(i) => (push_parse_stack (INITIALIZATION(i)) newctxt)
	      | NONE => raise ActionHandlerException("bad initialization")
	end;
        
    (* R 504 *)

    (* begin: push an empty entity decl list onto the parse stack *)
    fun entity_decl_list_begin kvs ctxt =
        push_parse_stack (ENTITY_DECLS ([])) ctxt;
        
    (* for each entity declaration, pop off any initializers and array specs *)
    fun entity_decl kvs ctxt =
        let fun check_decl_init ctxt =
		let val potentialInit = (peek_parse_stack ctxt)
		in
		    case potentialInit of
			SOME(INITIALIZATION(i)) => 
			let val (junk,newctxt) = (pop_parse_stack ctxt)
			in (SOME(i),newctxt) end
		      | _ => (NONE,ctxt)
		end;
	    fun check_decl_aspec ctxt =
		let val potentialArrayspec = (peek_absyn_stack ctxt)
		in
		    case potentialArrayspec of
			SOME(ABSYN.ARRAYSPEC(asl)) =>
			let val (junk,newctxt) = (pop_absyn_stack ctxt)
			in (SOME(ABSYN.ARRAYSPEC(asl)), newctxt) end
		      | _ => (NONE,ctxt)
		end;
	    val (ini, new_ctxt) = check_decl_init ctxt;
	    val (aspec, new_ctxt) = check_decl_aspec new_ctxt;
	    val (elt, new_ctxt) = (pop_parse_stack new_ctxt)
        in
            case elt of
                SOME(ENTITY_DECLS(eds)) =>
                let val t = get_key_value "id" kvs;
                in
                    case t of
                        SOME(AP.TOKEN(tt)) => 
			push_parse_stack
			    (ENTITY_DECLS(eds@
					  [ABSYN.ENTITY{name=(#id tt),
					                initializer=ini,
					                arrayspec=aspec}]))
			    new_ctxt
                      | _ => raise ActionHandlerException("Bogus id")
                end
              | _ => raise ActionHandlerException("Bad Parse Stack")
        end;
        
    fun entity_decl_list kvs ctxt =
        let fun check_decl_attrs ctxt =
		let val potentialAttrs = (peek_parse_stack ctxt)
		in
		    case potentialAttrs of
			SOME(ATTRS(attrs)) =>
			let val (junk,newctxt) = (pop_parse_stack ctxt)
			in (SOME(ATTRS(attrs)), newctxt) end
		      | _ => (NONE,ctxt)
		end;
	    val count = AP.extract_int (get_key_value "count" kvs)
            val (decls, new_ctxt) = (pop_parse_stack ctxt)
            val (attrs, new_ctxt) = (check_decl_attrs new_ctxt)
            val (ty, new_ctxt) = (pop_parse_stack new_ctxt)
        in
            case decls of
                SOME(ENTITY_DECLS(eds)) =>
                ( case ty of
		      SOME(TYPE(t)) =>
		      let val scope = (#c_scope new_ctxt)
                          val name_strings = 
			      map (fn ABSYN.ENTITY({name=n,...}) 
						   => n) eds;
		          val new_ctxt = 
			      {c_scope = (SYMTAB.listenter 
                                              (scope, name_strings, t)),
			       absyn_stack = (#absyn_stack new_ctxt),
			       parse_stack = (#parse_stack new_ctxt)}
			  val d_attrs = (case attrs of 
					     SOME(ATTRS(a)) => a
					   | NONE => []
					   | _ => raise ActionHandlerException("Unexpected attributes."))
		      in
			   push_absyn_stack 
                               (ABSYN.DECLARATION({attributes=d_attrs,
						   ty=t,
						   entities=eds}))
			       new_ctxt
		      end
		    | _ => raise ActionHandlerException("Bad Type")
                )
              | _ => raise ActionHandlerException("Bad Entity Decls")
        end;
	
    (* R 601 *)
    fun variable kv ctxt =
	let val (dref, new_ctxt) = (pop_absyn_stack ctxt)
	in
            case dref of
		SOME(ABSYN.DATAREF(dr)) =>
		(push_absyn_stack (ABSYN.VARIABLE(ABSYN.DATAREF(dr))) new_ctxt)
              | _ => raise ActionHandlerException("Bad Data Ref")
	end;

    (* R 705 *)
    fun extract_addop s =
	case s of
            SOME(AP.TOKEN(t)) =>
            let val ss = (#id t)
            in
		case ss of
		    "'+'" => AbstractSyntax.PLUS
		  | "'-'" => AbstractSyntax.MINUS
		  | _ => (TextIO.print(ss^"\n");
			  raise ActionHandlerException("Bad Addop"))
            end
	  | _ => raise ActionHandlerException("Bad Addop");
	
    fun extract_unaddop s =
	case s of
            SOME(AP.TOKEN(t)) =>
            let val ss = (#id t)
            in
		case ss of
		    "'-'" => AbstractSyntax.UMINUS
		  | "'+'" => AbstractSyntax.UPLUS
		  | _ => (TextIO.print(ss^" -- "^Int.toString((#line t))^
				       ":"^Int.toString((#col t))^
				       "\n");
			  raise ActionHandlerException("Bad Addop"))
            end
	  | _ => raise ActionHandlerException("Bad Addop");
	
    fun add_operand_add_op kv ctxt =
	let val (rhs,new_ctxt) = (pop_absyn_stack ctxt);
            val (lhs,new_ctxt) = (pop_absyn_stack new_ctxt);
            val addop = extract_addop (get_key_value "addOp" kv)
	in
            case rhs of
		SOME(r) => (case lhs of
				SOME(l) =>
				(push_absyn_stack
				     (ABSYN.BINARYOP({left=l,
						      operand=addop,
						      right=r}))
				     new_ctxt)
			      | _ => raise ActionHandlerException("Bad LHS")
			   )
	  | _ => raise ActionHandlerException("Bad RHS")
	end;

    (* TODO - this could probably made simpler using numAddOps *)
    fun add_operand kv ctxt =
	let val ao = (get_key_value "addOp" kv)
	in
            case ao of
		SOME(AP.STRING(s)) => ctxt
              | SOME(AP.TOKEN(t)) =>
		let val addop = extract_unaddop ao
		    val (theop,new_ctxt) = (pop_absyn_stack ctxt)
		    fun do_unop a =
			case a of
			    ABSYN.BINARYOP({left=l,
					    operand=opnd,
					    right=r}) =>
			    ABSYN.BINARYOP({left=(do_unop l),
					    operand=opnd,
					    right=r})
			  | _ => ABSYN.UNARYOP({node=a,
						operand=addop})
		in
		    case theop of
			SOME(ABSYN.BINARYOP(a)) =>
			(push_absyn_stack 
			     (do_unop (ABSYN.BINARYOP(a))) 
			     new_ctxt)
		      | SOME(n) => (push_absyn_stack
				    (ABSYN.UNARYOP({node=n,
						    operand=addop}))
				     new_ctxt)
		      | _ => (TextIO.print(Int.toString(#line t)^":"^
					   Int.toString(#col t));
			      raise ActionHandlerException("Bad Unop"))
		end
              | _ => raise ActionHandlerException("Bad Add Operand")
	end

    (* R 704 *)
    fun extract_multop s =
    case s of
        SOME(AP.TOKEN(t)) =>
        let val ss = (#id t)
        in
        case ss of
            "'*'" => AbstractSyntax.TIMES
          | "'/'" => AbstractSyntax.DIVIDE
          | _ => (TextIO.print(ss^"\n");raise ActionHandlerException("Bad Multop"))
        end
      | _ => raise ActionHandlerException("Bad Multop");

    fun mult_operand_mult_op kv ctxt =
    let val (rhs,new_ctxt) = (pop_absyn_stack ctxt);
        val (lhs,new_ctxt) = (pop_absyn_stack new_ctxt);
        val multop = extract_multop (get_key_value "multOp" kv)
    in
        case rhs of
        SOME(r) => (case lhs of
                SOME(l) =>
                  (push_absyn_stack
                       (ABSYN.BINARYOP({left=l,
                            operand=multop,
                            right=r}))
                       new_ctxt)
                  | _ => raise ActionHandlerException("Bad LHS")
               )
          | _ => raise ActionHandlerException("Bad RHS")
    end;

    fun extract_lvl3 s =
    case s of
        SOME(AP.TOKEN(t)) =>
        let val ss = (String.map (Char.toUpper) (#id t))
        in
        case ss of
            "'>'" => ABSYN.GREATERTHAN
          | "'.GT.'" => ABSYN.GREATERTHAN
          | "'<'" => ABSYN.LESSTHAN
          | "'.LT.'" => ABSYN.LESSTHAN
          | "'>='" => ABSYN.GREATEREQUAL
          | "'.GE.'" => ABSYN.GREATEREQUAL
          | "'<='" => ABSYN.LESSEQUAL
          | "'.LE.'" => ABSYN.LESSEQUAL
          | "'=='" => ABSYN.EQUALS
          | "'.EQ.'" => ABSYN.EQUALS
          | "'/='" => ABSYN.NOTEQUALS
          | "'.NE.'" => ABSYN.NOTEQUALS
          | _ => raise ActionHandlerException("Bad lvl3 op token")
        end
      | _ => raise ActionHandlerException("Bad lvl3 op")

    fun level_3_expr kv ctxt =
    let val (rhs,new_ctxt) = (pop_absyn_stack ctxt);
        val (lhs,new_ctxt) = (pop_absyn_stack new_ctxt);
        val relop = extract_lvl3 (get_key_value "relOp" kv)
    in
        case rhs of
        SOME(r) => (case lhs of
                SOME(l) =>
                (push_absyn_stack
                     (ABSYN.RELOP({left=l,
                           operand=relop,
                           right=r}))
                     new_ctxt)
                  | _ => raise ActionHandlerException("Bad LHS")
               )
          | _ => raise ActionHandlerException("Bad RHS")
    end;

    (* R 704 *)
    fun power_operand kv ctxt =
    let val (rhs,new_ctxt) = (pop_absyn_stack ctxt);
        val (lhs,new_ctxt) = (pop_absyn_stack new_ctxt);
    in
        case rhs of
        SOME(r) => (case lhs of
                SOME(l) =>
                (push_absyn_stack
                     (ABSYN.BINARYOP({left=l,
				      operand=ABSYN.POWER,
				      right=r}))
                     new_ctxt)
                  | _ => raise ActionHandlerException("Bad LHS")
               )
          | _ => raise ActionHandlerException("Bad RHS")
    end;

    (* R 734 *)
    fun assignment_stmt kv ctxt =
    let val (rhs, new_ctxt) = (pop_absyn_stack ctxt);
        val (lhs, new_ctxt) = (pop_absyn_stack new_ctxt)
    in
        case rhs of
        SOME(r) => (case lhs of
                SOME(l) =>
                  (push_absyn_stack
                       (ABSYN.ASSIGN({lhs=l,
                              rhs=r}))
                       new_ctxt)
                  | _ => raise ActionHandlerException("Bad LHS")
               )
          | _ => raise ActionHandlerException("Bad RHS")
    end;

    (* R 501 *)
    fun type_declaration_stmt kv ctxt =
        ctxt;

    (* R 502 *)
    fun declaration_type_spec kv ctxt =
	ctxt;

    (* R 1101 *)
    fun main_program_begin kv ctxt =
	push_absyn_stack (ABSYN.PROGSTMT({name=NONE,
					  scope=(#c_scope ctxt),
					  body=ABSYN.BLOCK([])})) ctxt;

    fun program_stmt kv ctxt =
    (* TODO - handle name properly (if not present) *)
    let val name = AP.extract_token (get_key_value "id" kv);
        val (ps,new_ctxt) = (pop_absyn_stack ctxt)
    in
        case ps of
        SOME(ABSYN.PROGSTMT({name=n,
                     scope=s,
                     body=b})) =>
        (push_absyn_stack (ABSYN.PROGSTMT({name=SOME(#id name),
                           scope=s,
                           body=b}))
         new_ctxt)
          | _ => raise ActionHandlerException("Bad Program Stmt")
    end;

    (* R 831 *)
    fun do_variable kv ctxt = 
	ctxt;

    (* R 830 *)
    fun loop_control kv ctxt =
	ctxt;

    (* R 827 *)
    fun do_stmt kv ctxt =
	let val hasLoopControl = AP.extract_bool
				     (get_key_value "hasLoopControl" kv);
            fun extractoption opt =
		case opt of
		    SOME(a) => a
		  | _ => raise ActionHandlerException("Bad option")
	in
            if hasLoopControl then
		let val (n1, new_ctxt) = (pop_absyn_stack ctxt);
		    val (n2, new_ctxt) = (pop_absyn_stack new_ctxt);
		    val (n3, new_ctxt) = (pop_absyn_stack new_ctxt)
		in
		    case n3 of
			SOME(ABSYN.VARIABLE(v)) =>
			(push_absyn_stack
			     (ABSYN.DOLOOP(
			      {ctrl=
			       SOME(ABSYN.ITERATE({var=ABSYN.VARIABLE(v),
						   lo=(extractoption n2),
						   hi=(extractoption n1),
						   inc=NONE})),
			       body=ABSYN.BLOCK([]),
			       label=NONE,
			       closed=false}))
			     new_ctxt)
		      | SOME(_) =>
			let val (n4, new_ctxt) = (pop_absyn_stack new_ctxt)
			in case n4 of
			       SOME(ABSYN.VARIABLE(v)) =>
			       (push_absyn_stack
				    (ABSYN.DOLOOP(
				     {ctrl=
				      SOME(ABSYN.ITERATE(
					   {var=ABSYN.VARIABLE(v),
					    lo=(extractoption n3),
					    hi=(extractoption n2),
					    inc=SOME((extractoption n1))
					  })),
				      body=ABSYN.BLOCK([]),
				      label=NONE,
				      closed=false}))
				    new_ctxt)
			     | _ => raise ActionHandlerException("Bad Do 2")
			end
		      | _ => raise ActionHandlerException("Bad Do Control")
		end
            else
		(push_absyn_stack (ABSYN.DOLOOP({ctrl=NONE,
						 body=ABSYN.BLOCK([]),
						 label=NONE,
						 closed=false})) ctxt)
	end;
	
    (* R 834 *)
    fun end_do_stmt kv ctxt =
	let fun pop_body context body =
		let val (elt,new_ctxt) = (pop_absyn_stack context)
		in
		    case elt of
			SOME(ABSYN.DOLOOP({ctrl=c, body=b, 
					   closed=cl, label=lbl})) =>
			if cl=true then
			    (pop_body new_ctxt
				      ((ABSYN.DOLOOP({ctrl=c,
						      body=b,
						      label=lbl,
						      closed=cl}))::body))
			else
			    (push_absyn_stack
				 (ABSYN.DOLOOP({ctrl=c,
						body=ABSYN.BLOCK(body),
						label=lbl,
						closed=true}))
				 new_ctxt)
		      | SOME(othertype) => 
			(* GROSS HACK *)
			(case othertype of
			     ABSYN.BBLOCK(ABSYN.BLOCK(b)) =>
			     (pop_body new_ctxt (b@body))
			   | _ => (pop_body new_ctxt (othertype::body))
			)
		      | _ => raise ActionHandlerException("Bad End Do")
		end
	in
            (pop_body ctxt [])
	end;

    (* R 313 *)
    fun handle_label kv ctxt = 
	let val label_name = AP.extract_token (get_key_value "lbl" kv)
	in
            push_parse_stack (LABEL(label_name)) ctxt
	end

    (* R 848 *)
    fun continue_stmt kv ctxt =
	let val (ps_top, newctxt) = (pop_parse_stack ctxt)
	    val (lbl, newctxt) = case ps_top of
				     (* a label was on the parse stack *)
				     SOME(LABEL(l)) =>
				     let val lstr = #id l
				     in
					 (SOME(lstr),newctxt)
				     end
				   | SOME(other) => (* not a label on stack *)
				     let val newctxt = 
					     (push_parse_stack other newctxt)
				     in
					 (NONE, newctxt)
				     end
				   | _ => (* nothing on stack *)
				     (NONE, newctxt)
	    fun pop_body context label body =
		let val (elt,new_ctxt) = (pop_absyn_stack context)
		in
		    case elt of
			SOME(ABSYN.DOLOOP({ctrl=c, body=b, 
					   closed=cl, label=lbl})) =>
			if cl=true then
			    (pop_body new_ctxt label
				      ((ABSYN.DOLOOP({ctrl=c,
						      body=b,
						      label=label,
						      closed=cl}))::body))
			else
			    (push_absyn_stack
				 (ABSYN.DOLOOP({ctrl=c,
						body=ABSYN.BLOCK(body),
						label=label,
						closed=true}))
				 new_ctxt)
		      | SOME(othertype) => 
			(* GROSS HACK *)
			(case othertype of
			     ABSYN.BBLOCK(ABSYN.BLOCK(b)) =>
			     (pop_body new_ctxt label (b@body))
			   | _ => (pop_body new_ctxt label (othertype::body))
			)
		      | _ => raise ActionHandlerException("Bad End Do")
		end
	in
	    (pop_body newctxt lbl [])
	end;

    (* R 1103 *)
    fun end_program_stmt kv ctxt =
     (* TODO - get name and handle name=null token properly *)
     let fun pop_body context body =
        let val (elt,new_ctxt) = (pop_absyn_stack context)
        in
            case elt of
                SOME(ABSYN.PROGSTMT(ps)) =>
                (push_absyn_stack
                     (ABSYN.PROGSTMT({name=(#name ps),
                                      scope=(#c_scope new_ctxt),
                                      body=ABSYN.BLOCK(body)}))
                     new_ctxt)
              | SOME(othertype) => (pop_body new_ctxt
                                             (othertype::body))
              | _ => raise ActionHandlerException("Bad Prog. Stmt")
        end
    in
        (pop_body ctxt [])
    end;
    
    (* R 803 *)
    fun if_then_stmt kv ctxt =
	let val (c,newctxt) = (pop_absyn_stack ctxt)
	in
	    case c of
		SOME(cc) => 
		(push_absyn_stack (ABSYN.IFTHENELSE({cond=cc, 
						     then_clause=NONE,
						     else_clause=NONE,
						     closed=false,
						     elseif=false}))
				  newctxt)
	      | _ => raise ActionHandlerException("Bad If Condition")
	end;

    (* R 805 *)
    fun else_stmt kv ctxt =
	let val (body,newctxt) = (pop_absyn_stack ctxt);
	    val (ite,newctxt) = (pop_absyn_stack newctxt)
	in
	    case ite of
		SOME(ABSYN.IFTHENELSE(i)) =>
		(case body of
		     SOME(ABSYN.BBLOCK(b)) =>
		     push_absyn_stack 
			 (ABSYN.IFTHENELSE({cond=(#cond i),
					    then_clause = SOME(b),
					    else_clause = NONE,
					    closed=false,
					    elseif=(#elseif i)})) newctxt
		   | _ => raise ActionHandlerException("Bad then block")
		)
	      | _ => raise ActionHandlerException("Malformed ifthenelse")
	end;

    (* R 804 *)
    fun else_if_stmt kv ctxt =
	let val (cond,newctxt) = (pop_absyn_stack ctxt);
	    val (body,newctxt) = (pop_absyn_stack newctxt);
	    val (ite,newctxt) = (pop_absyn_stack newctxt)
	in
	    case ite of
		SOME(ABSYN.IFTHENELSE(i)) =>
		(case body of
		     SOME(ABSYN.BBLOCK(b)) =>
		     let val newctxt = 
			     push_absyn_stack 
				 (ABSYN.IFTHENELSE({cond=(#cond i),
						    then_clause = SOME(b),
						    else_clause = NONE,
						    closed=false,
						    elseif=(#elseif i)})) 
				 newctxt
		     in
			 case cond of
			     SOME(c) => 
			     (push_absyn_stack 
				  (ABSYN.IFTHENELSE({cond=c,
						     then_clause = NONE,
						     else_clause = NONE,
						     closed=false,
						     elseif=true}))
				  newctxt)
			   | _ => 
			     raise ActionHandlerException("Bad elseifstmt")
		     end
		   | _ => raise ActionHandlerException("Bad then block")
		)
	      | _ => raise ActionHandlerException("Malformed ifthenelse")
	end;
        
    (* R 806 *)
    fun handle_elseifs ctxt =
	let val (ite,newctxt) = (pop_absyn_stack ctxt);
	    val (pite,newctxt) = (pop_absyn_stack newctxt)
	in
	    case pite of
		SOME(ABSYN.IFTHENELSE(pi)) =>
		( case ite of
		      SOME(ABSYN.IFTHENELSE(i)) =>
		      let val elseblock = ABSYN.BLOCK([ABSYN.IFTHENELSE(i)]);
			  val newctxt =
			      push_absyn_stack
			      (ABSYN.IFTHENELSE({cond=(#cond pi),
						 then_clause=(#then_clause pi),
						 else_clause=SOME(elseblock),
						 closed=true,
						 elseif=(#elseif pi)}))
			      newctxt
		      in
			  if (#elseif pi)=true then
			      handle_elseifs newctxt
			  else
			      newctxt
		      end
		    | _ => raise ActionHandlerException("Bad ITE")
		)
	      | _ => raise ActionHandlerException("Bad contained ITE")
	end;

    fun end_if_stmt kv ctxt =
	let val (body,newctxt) = (pop_absyn_stack ctxt);
	    val (ite,newctxt) = (pop_absyn_stack newctxt)
	in
	    case ite of
		SOME(ABSYN.IFTHENELSE(i)) =>
		(case body of
		     SOME(ABSYN.BBLOCK(b)) =>
		     (let val newctxt = 
			  case (#then_clause i) of
			  NONE =>
			  push_absyn_stack 
			      (ABSYN.IFTHENELSE({cond=(#cond i),
						 then_clause = SOME(b),
						 else_clause = NONE,
						 closed=true,
						 elseif=(#elseif i)})) newctxt
			| _ => 
			  push_absyn_stack 
			      (ABSYN.IFTHENELSE({cond=(#cond i),
						 then_clause=(#then_clause i),
						 else_clause = SOME(b),
						 closed=true,
						 elseif=(#elseif i)})) newctxt
		      in
			  if (#elseif i)=true then
			      handle_elseifs newctxt
			  else
			      newctxt
		      end
		     )
		   | _ => raise ActionHandlerException("Bad then block")
		)
	      | _ => raise ActionHandlerException("Malformed ifthenelse")
	end;

    (* R1220 *)
    fun actual_arg_spec kvs ctxt =
        (* not sure why a token can't be extracted directly? *)
        let val keystr = AP.extract_string (get_key_value "keyword" kvs);
            val key = case (AP.tokenValue keystr) of
                          AP.STRING(ss) => ss
                        | _ => raise ActionHandlerException("Bad keyword");
            val key = ABSYN.STRINGCONST(key);
	    val (op_arg,newctxt) = pop_absyn_stack ctxt;
	    val (ss_arg_list,newctxt) = (pop_parse_stack newctxt);
            val ss_arg = case op_arg of
                             SOME(a) =>
                                 SS_ARG({lo=NONE,
                                         hi=NONE,
                                         stride=NONE,
                                         arg=SOME(ABSYN.ACTUALARG(
                                                {arg=a, keyword=SOME(key)}))})
			   | _ => raise ActionHandlerException("Bad arg")
	in
	    case ss_arg_list of
		SOME(SS_ARG_LIST(l)) =>
		(push_parse_stack (SS_ARG_LIST(l@[ss_arg]))
		 newctxt)
	      | _ => raise ActionHandlerException("Bogus section subscript")
	end;

    fun section_subscript_list_begin kvs ctxt =
	(push_parse_stack (SS_ARG_LIST([])) ctxt);

    fun section_subscript kvs ctxt = 
	let val hasLo = AP.extract_bool (get_key_value "hasLowerBound" kvs);
	    val hasHi = AP.extract_bool (get_key_value "hasUpperBound" kvs);
	    val hasSt = AP.extract_bool (get_key_value "hasStride" kvs);
	    val (St,newctxt) = if (hasSt) then (pop_absyn_stack ctxt)
			       else (NONE,ctxt);
	    val (Hi,newctxt) = if (hasHi) then (pop_absyn_stack newctxt)
			       else (NONE,newctxt);
	    val (Lo,newctxt) = if (hasLo) then (pop_absyn_stack newctxt)
			       else (NONE,newctxt);
	    val (sslist,newctxt) = (pop_parse_stack newctxt)
	in
	    case sslist of
		SOME(SS_ARG_LIST(l)) =>
		(push_parse_stack (SS_ARG_LIST(l@[SS_ARG({lo=Lo,
						          hi=Hi,
						          stride=St,
                                                          arg=NONE})]))
		 newctxt)
	      | _ => raise ActionHandlerException("Bogus section subscript")
	end;

    (* R1228 *)
    fun t_prefix_spec kvs ctxt =
        let val tktype = #ty (AP.extract_token (get_key_value "spec" kvs))
            val pref = 
                case tktype of
	            TK.T_RECURSIVE => PROC_ATTR ABSYN.PROC_RECURSIVE
                  | TK.T_PURE      => PROC_ATTR ABSYN.PROC_PURE
                  | TK.T_ELEMENTAL => PROC_ATTR ABSYN.PROC_ELEMENTAL
                  | _ => raise ActionHandlerException("Bad prefix-spec")
	in
            push_parse_stack pref ctxt
	end;

    (* R1228 *)
    fun prefix_spec kvs ctxt =
	let val hasDecl = AP.extract_bool (get_key_value "isDecTypeSpec" kvs)
        in
            if (hasDecl) then
                let val (ty,newctxt) = pop_parse_stack ctxt
                in
                    case ty of
                       SOME(TYPE(t)) =>
                          push_parse_stack
                              (PROC_ATTR (ABSYN.PROC_TYPESPEC t)) newctxt
                     | _ => raise ActionHandlerException("Bad prefix_spec type")
                end
            else
                (* t_prefix_spec has already put PROC_ATTR in parse stack *)
                ctxt
        end;

    (* R1227 *)
    fun prefix kvs ctxt =
        let fun addprefix 0 plist context = (plist,context)
              | addprefix i plist context =
                    let val (elt,new_ctxt) = (pop_parse_stack context);
                        val p = case elt of
                                    SOME(PROC_ATTR(prefx)) => prefx
                                  | _ => raise
                                      ActionHandlerException("Bad Parse Stack")
                    in
                        addprefix (i-1) ([p]@plist) new_ctxt
                    end;
            val count = AP.extract_int (get_key_value "specCount" kvs)
            val (plist,newctxt) = addprefix count [] ctxt
        in
            push_parse_stack (PROCATTRLIST(plist)) newctxt
        end;

    (* R1233 *)
    fun dummy_arg_list_begin kvs ctxt = 
        (push_parse_stack (DUMMYARGLIST([])) ctxt);

    fun dummy_arg kvs ctxt =
        let val (elt,new_ctxt) = (pop_parse_stack ctxt)
        in
            case elt of
                SOME(DUMMYARGLIST(dalist)) =>
                (let val daval = AP.extract_token (get_key_value "dummy" kvs)
                 in
                     (push_parse_stack
                          (DUMMYARGLIST(dalist@[dequote (#id daval)]))
                          new_ctxt)
                 end)
              | _ => raise ActionHandlerException("Bad Parse Stack")
        end;

    (* R1232 *)
    fun subroutine_stmt kvs ctxt =
        let val sname = dequote
                          (#id (AP.extract_token (get_key_value "name" kvs)));
            val hasBindSpec = AP.extract_bool 
                                (get_key_value "hasBindingSpec" kvs);
            val hasDAList = AP.extract_bool 
                                (get_key_value "hasDummyArgList" kvs);
            val hasPrefix = AP.extract_bool 
                                (get_key_value "hasPrefix" kvs);
            val (bspec,newctxt) = if (hasBindSpec) then (pop_parse_stack ctxt)
                                  else (NONE, ctxt);
            val (dalist,newctxt) = if (hasDAList) then (pop_parse_stack newctxt)
                                   else (NONE, newctxt);
            val (prelist,newctxt) = if (hasPrefix) then
                                        (pop_parse_stack newctxt)
                                    else (NONE, newctxt);
            val blist = 
                ( case bspec of
                      SOME(PROC_ATTR(b)) => [b]
                    | NONE               => []
                    | _ => raise ActionHandlerException("Bad Parse Stack")
                );
            val dlist = case dalist of
                            SOME(DUMMYARGLIST(l)) => l
                          | NONE => []
                          | _ => raise ActionHandlerException("Bad Parse Stack")
            val plist = case prelist of
                            SOME(PROCATTRLIST(l)) => l
                          | NONE => []
                          | _ => raise ActionHandlerException("Bad Parse Stack")
        in
            (push_absyn_stack (ABSYN.SUBROUTINE({name=sname,
                                                 attrs=(plist@blist),
                                                 args=dlist,
                                                 scope=(#c_scope newctxt),
                                                 body=(ABSYN.BLOCK([]))}))
             newctxt)
        end;

    fun end_subroutine_stmt kvs ctxt =
        (* TODO - get name and handle name=null token properly *)
        let val sname = AP.extract_token (get_key_value "name" kvs);
            fun pop_body context body =
                let val (elt,new_ctxt) = (pop_absyn_stack context)
                (* TODO - ERRCHK - make sure names agree *)
                in
                    case elt of
                        SOME(ABSYN.SUBROUTINE(sr)) =>
                        (push_absyn_stack
                             (ABSYN.SUBROUTINE({scope=(#c_scope new_ctxt):symtab,
                                                body=ABSYN.BLOCK(body),
                                                name=(#name sr),
                                                args=(#args sr),
                                                attrs=(#attrs sr)}))
                             new_ctxt)
                      | SOME(othertype) => (pop_body new_ctxt
                                                     (othertype::body))
                      | _ => raise ActionHandlerException("Bad Subr. Stmt")
                end
        in
            (pop_body ctxt [])
        end;

    (* NOTE: ignoring label for now *)
    fun cycle_stmt kvs ctxt =
        push_absyn_stack (ABSYN.CYCLE({name=NONE})) ctxt;

    fun array_spec_begin kv ctxt =
	push_absyn_stack (ABSYN.ARRAYSPEC([])) ctxt;

    fun array_spec kvs ctxt =
	ctxt;

    fun attr_spec kvs ctxt =
	let val aspec = ABSYN.attrSpecFromAction kvs;
	    fun check_decl_aspec ctxt =
		let val potentialAttrspec = (peek_parse_stack ctxt)
		in
		    case potentialAttrspec of
			SOME(ATTRS(asl)) =>
			let val (junk,newctxt) = (pop_parse_stack ctxt)
			in (asl, newctxt) end
		      | _ => ([], ctxt)
		end;
	    val (attrs,new_ctxt) = check_decl_aspec ctxt
	in
	    case aspec of
		ABSYN.ATTRSPEC_DIMENSION =>
		let val (arrspec,new_ctxt) = (pop_absyn_stack new_ctxt)
		in
		    case arrspec of
			SOME(ABSYN.ARRAYSPEC(a)) => 
			push_parse_stack
			    (ATTRS(attrs@[ABSYN.ATTR_DIMENSION(a)])) 
			    new_ctxt
		      | _ => raise ActionHandlerException("Bad dimension.")
		end
	      | ABSYN.ATTRSPEC_PARAMETER =>
		push_parse_stack 
		    (ATTRS(attrs@[ABSYN.ATTR_PARAMETER]))
		    new_ctxt
	      | ABSYN.ATTRSPEC_INTENT =>
		let val (itype, new_ctxt) = (pop_parse_stack new_ctxt)
		in
		    case itype of
			SOME(INTENT(i)) =>
			push_parse_stack
			    (ATTRS(attrs@[ABSYN.ATTR_INTENT(i)]))
			    new_ctxt
		      | _ => raise ActionHandlerException("Bad intent.")
		end
	      | _ => raise ActionHandlerException("Unsupported attr type.")
	end;

    fun array_spec_element kvs ctxt =
	let val etype = ABSYN.arraySpecTypeFromAction kvs;
	    fun get_expr ctxt =
		let val (e,newctxt) = (pop_absyn_stack ctxt)
		in case e of
		       SOME(ee) => (ee,newctxt)
		     | NONE => raise ActionHandlerException("Bad Expr")
		end;

	    fun get_arrayspec ctxt =
		let val potentialArrayspec = (peek_absyn_stack ctxt)
		in
		    case potentialArrayspec of
			SOME(ABSYN.ARRAYSPEC(asl)) =>
			let val (junk,newctxt) = (pop_absyn_stack ctxt)
			in (ABSYN.ARRAYSPEC(asl), newctxt) end
		      | _ => (ABSYN.ARRAYSPEC([]),ctxt)
		end
	in
	    case etype of
		ABSYN.ASPEC_EXPR =>
		let val (e,newctxt) = (get_expr ctxt);
		    val (retval,newctxt) = 
			(get_arrayspec newctxt);
		    val aa = case retval of
				 ABSYN.ARRAYSPEC(aaa) => aaa
			       | _ => raise ActionHandlerException("BADASPEC");
		    val ae = ABSYN.AS_EXPR(e)
		in
		    (push_absyn_stack (ABSYN.ARRAYSPEC(aa@[ae])) newctxt)
		end
	      | ABSYN.ASPEC_EXPR_COLON =>
		let val (e,newctxt) = (get_expr ctxt);
		    val (retval,newctxt) = 
			(get_arrayspec newctxt);
		    val aa = case retval of
				 ABSYN.ARRAYSPEC(aaa) => aaa
			       | _ => raise ActionHandlerException("BADASPEC");
		    val ae = ABSYN.AS_EXPR_COLON(e)
		in
		    (push_absyn_stack (ABSYN.ARRAYSPEC(aa@[ae])) newctxt)
		end
	      | ABSYN.ASPEC_EXPR_COLON_EXPR =>
		let val (e2,newctxt) = (get_expr ctxt);
		    val (e,newctxt) = (get_expr newctxt);
		    val (retval,newctxt) = 
			(get_arrayspec newctxt);
		    val aa = case retval of
				 ABSYN.ARRAYSPEC(aaa) => aaa
			       | _ => raise ActionHandlerException("BADASPEC");
 	            val ae = ABSYN.AS_EXPR_COLON_EXPR(e,e2)
		in
		    (push_absyn_stack (ABSYN.ARRAYSPEC(aa@[ae])) newctxt)
		end
	      | ABSYN.ASPEC_EXPR_COLON_ASTERISK =>
		let val (e,newctxt) = (get_expr ctxt);
		    val (retval,newctxt) = 
			(get_arrayspec newctxt);
		    val aa = case retval of
				 ABSYN.ARRAYSPEC(aaa) => aaa
			       | _ => raise ActionHandlerException("BADASPEC");
		    val ae = ABSYN.AS_EXPR_COLON_ASTERISK(e)
		in
		    (push_absyn_stack (ABSYN.ARRAYSPEC(aa@[ae])) newctxt)
		end
	      | ABSYN.ASPEC_ASTERISK =>
		let val (retval,newctxt) = 
			(get_arrayspec ctxt);
		    val aa = case retval of
				 ABSYN.ARRAYSPEC(aaa) => aaa
			       | _ => raise ActionHandlerException("BADASPEC");
		    val ae = ABSYN.AS_ASTERISK
		in
		    (push_absyn_stack (ABSYN.ARRAYSPEC(aa@[ae])) newctxt)
		end
	      | ABSYN.ASPEC_COLON =>
		let val (retval,newctxt) = 
			(get_arrayspec ctxt);
		    val aa = case retval of
				 ABSYN.ARRAYSPEC(aaa) => aaa
			       | _ => raise ActionHandlerException("BADASPEC");
		    val ae = ABSYN.AS_COLON
		in
		    (push_absyn_stack (ABSYN.ARRAYSPEC(aa@[ae])) newctxt)
		end
	end;

    fun letter_spec kvs ctxt =
	let fun check_letter_list ctxt =
		let val potentialList = (peek_parse_stack ctxt)
		in
		    case potentialList of
			SOME(LETTER_LIST(ll)) => 
			let val (junk,newctxt) = (pop_parse_stack ctxt)
			in (SOME(ll),newctxt) end
		      | _ => (NONE,ctxt)
		end;
	    fun optionize_kval kstr =
		let val keyval = get_key_value kstr kvs
		in 
		    case keyval of
			SOME(AP.STRING("null")) => NONE
		      | SOME(AP.TOKEN(tt)) => SOME(AP.extract_token keyval)
		      | _ => raise ActionHandlerException("Bad kval")
		end;
	    val id1 = optionize_kval "id1"
	    val id2 = optionize_kval "id2"
	    val id1val = case id1 of
			     SOME(s) => (#id s)
			   | _ => raise ActionHandlerException("Bad token");
	    val id2val = case id2 of
			     SOME(s) => SOME (#id s)
			   | _ => NONE;
	    val (ll, new_ctxt) = check_letter_list ctxt
	in
	    case ll of
		SOME(llist) => push_parse_stack 
				   (LETTER_LIST(llist@[{id1=id1val,
							id2=id2val}]))
				   new_ctxt
	      | NONE => push_parse_stack 
			    (LETTER_LIST([{id1=id1val,id2=id2val}]))
			    new_ctxt
	end;

    fun call_stmt kvs ctxt =
	let val (proc, new_ctxt) = (pop_absyn_stack ctxt)
	in
	    case proc of
		SOME(n) => push_absyn_stack (ABSYN.CALLSTMT(n)) new_ctxt
	      | _ => raise ActionHandlerException("Bad call")
	end;

    (* R 549 *)
    (* TODO - implement case when hasImplicitSpecList *)
    fun implicit_stmt kvs ctxt =
        let val hasISList = AP.extract_bool 
                                (get_key_value "hasImplicitSpecList" kvs);
        in
            if (hasISList) then
                raise ActionHandlerException("ImplicitSpecList Unimplemented")
            else (push_absyn_stack (ABSYN.IMPLICIT_STMT(ABSYN.IMPLICIT_NONE))
                  ctxt)
        end;

    (* R 550 *)
    fun implicit_spec kvs ctxt =
	let val (ll_opt, new_ctxt) = (pop_parse_stack ctxt);
	    val (ity_opt, new_ctxt) = (pop_parse_stack new_ctxt);
	    val ll = case ll_opt of
			 SOME(LETTER_LIST(l)) => l
		       | _ => raise ActionHandlerException("Bad");
	    val ity = case ity_opt of
			  SOME(TYPE(i)) => i
			| _ => raise ActionHandlerException("Bad");
	    fun check_implicit_stmt ctxt =
		let val potentialImplicit = (peek_absyn_stack ctxt)
		in
		    case potentialImplicit of
			SOME(ABSYN.IMPLICIT_STMT(i)) => 
			let val (junk,newctxt) = (pop_absyn_stack ctxt)
			in (SOME(i),newctxt) end
		      | _ => (NONE,ctxt)
		end;
	    val (istmt, new_ctxt) = check_implicit_stmt new_ctxt
	in
	    case istmt of
		SOME(ABSYN.IMPLICIT_SPEC(i)) => 
		push_absyn_stack
		    (ABSYN.IMPLICIT_STMT(ABSYN.IMPLICIT_SPEC(i@[{ty=ity,
								letters=ll}])))
		    new_ctxt
	      | NONE => push_absyn_stack
			(ABSYN.IMPLICIT_STMT(ABSYN.IMPLICIT_SPEC([{ty=ity,
								   letters=ll}])))
			new_ctxt
	      | _ => raise ActionHandlerException("Bad implicit combination.")
	end;

    fun intent_spec kvs ctxt = 
	let val intent = ABSYN.intentSpecFromAction kvs
	in
	    push_parse_stack (INTENT(intent)) ctxt
	end;

    fun where_stmt kvs ctxt =
	let val (assn, new_ctxt) = (pop_absyn_stack ctxt);
	    val (cond, new_ctxt) = (pop_absyn_stack new_ctxt)
	    val assign = case assn of
			     SOME(ABSYN.ASSIGN(a)) =>
                                 ABSYN.ASSIGN({lhs=(#lhs a), rhs=(#rhs a)})
			   | _ => raise ActionHandlerException("Bad Where");
	in
	    case cond of
		SOME(c) => push_absyn_stack
			       (ABSYN.WHERE({cond=c,
					     body=ABSYN.BLOCK([assign]),
					     else_clause=NONE,
					     bclosed=true,
					     eclosed=true}))
			       new_ctxt
	      | _ => raise ActionHandlerException("Bad where")
	end;

    fun where_construct_stmt kvs ctxt =
	let val (cond, new_ctxt) = (pop_absyn_stack ctxt)
	in
	    case cond of
		SOME(c) => push_absyn_stack
			       (ABSYN.WHERE({cond=c,
					     body=ABSYN.BLOCK([]),
					     else_clause=NONE,
					     bclosed=false,
					     eclosed=false}))
			       new_ctxt
	      | _ => raise ActionHandlerException("Bad where")
	end;

    fun where_body_construct kvs ctxt =
	let val (body_elt, new_ctxt) = (pop_absyn_stack ctxt);
	    val body_el = 
		(case body_elt of
		     SOME(b) => b
		   | NONE => raise ActionHandlerException("Bad Body Element"));
	    val (ws, new_ctxt) = (pop_absyn_stack new_ctxt)
	in
	    case ws of
		SOME(ABSYN.WHERE({cond=c,body=ABSYN.BLOCK(b),
				  else_clause=elseclause,
				  bclosed=bcflag,eclosed=ecflag})) =>
		if bcflag=false then
		    push_absyn_stack
			(ABSYN.WHERE({cond=c,
				      body=ABSYN.BLOCK(b@[body_el]),
				      else_clause=elseclause,
				      bclosed=false,eclosed=ecflag}))
			new_ctxt
		else
		    if ecflag=false then
			case elseclause of
			    NONE =>
			    push_absyn_stack
				(ABSYN.WHERE({cond=c,
					      body=ABSYN.BLOCK(b),
					      else_clause=
					      SOME(ABSYN.BLOCK([body_el])),
					      bclosed=true,eclosed=false}))
				new_ctxt
			  | SOME(ABSYN.BLOCK(e)) =>
			    push_absyn_stack
				(ABSYN.WHERE({cond=c,body=ABSYN.BLOCK(b),
					      else_clause=
					      SOME(ABSYN.BLOCK(e@[body_el])),
					      bclosed=true,eclosed=false}))
				new_ctxt
		    else
			raise ActionHandlerException("Bad while.")
	      | _ => raise ActionHandlerException("Unexpected body cons.")
	end;

    fun elsewhere_stmt kvs ctxt =
	let val (w,new_ctxt) = (pop_absyn_stack ctxt)
	in
	    case w of
		SOME(ABSYN.WHERE({cond=c,body=b,else_clause=e,
				  bclosed=bc,eclosed=ec})) =>
		push_absyn_stack
		    (ABSYN.WHERE({cond=c,body=b,else_clause=e,
				  bclosed=true,eclosed=ec}))
		    new_ctxt
	      | _ => raise ActionHandlerException("Bad elsewhere")
	end;

    fun elsewhere_stmt_end kvs ctxt =
	let val (w,new_ctxt) = (pop_absyn_stack ctxt)
	in
	    case w of
		SOME(ABSYN.WHERE({cond=c,body=b,else_clause=e,
				  bclosed=bc,eclosed=ec})) =>
		push_absyn_stack
		    (ABSYN.WHERE({cond=c,body=b,else_clause=e,
				  bclosed=true,eclosed=true}))
		    new_ctxt
	      | _ => raise ActionHandlerException("Bad elsewhere end")
	end;
	
    fun end_where_stmt kvs ctxt =
	let val (w, new_ctxt) = (pop_absyn_stack ctxt)
	in
	    case w of
		SOME(ABSYN.WHERE({cond=c,body=b,else_clause=e,
				  bclosed=bc,eclosed=ec})) =>
		push_absyn_stack
		    (ABSYN.WHERE({cond=c,body=b,else_clause=e,
				  bclosed=bc,eclosed=true}))
		    new_ctxt
	      | _ => raise ActionHandlerException("Bad endwhere")
	end;

    fun output_item kvs ctxt =
	let val potential_outputitems = (peek_parse_stack ctxt);
	    val (outputitems, newctxt) =
		case potential_outputitems of
		    SOME(OUTPUTITEMS(oi)) =>
		    let val (junk,newctxt) = (pop_parse_stack ctxt)
		    in (oi,newctxt) end
		  | _ => ([], ctxt);
	    val (outitem, newctxt) = (pop_absyn_stack newctxt)
	in
	    case outitem of
		SOME(i) => (push_parse_stack
			    (OUTPUTITEMS(outputitems@[i]))
			    newctxt)
	      | _ => raise ActionHandlerException("Bad Output Items")
	end;

    fun print_stmt kvs ctxt = 
	let val hasOutputItemList = AP.extract_bool (get_key_value
						     "hasOutputItemList"
						     kvs)
	in
	    if hasOutputItemList then
		let val (oilist, newctxt) = (pop_parse_stack ctxt)
		in
		    case oilist of
			SOME(OUTPUTITEMS(oi)) =>
			  (push_absyn_stack (ABSYN.PRINT(oi)) newctxt)
		      | _ => raise ActionHandlerException("Bad print OIL")
		end
	    else
		(push_absyn_stack (ABSYN.PRINT([])) ctxt)
	end;

    fun executable_construct kvs ctxt =
	let val (ex,newctxt) = (pop_absyn_stack ctxt)
	in
	    case ex of
		SOME(i) => (push_absyn_stack (ABSYN.EXECCONS(i)) newctxt)
	      | _ => raise ActionHandlerException("Bad exec. construct")
	end;

    fun block_handler kvs ctxt =
	let fun pop_block pctxt (ABSYN.BLOCK(b)) =
		let val top = peek_absyn_stack pctxt
		in
		    case top of
			SOME(ABSYN.EXECCONS(e)) =>
			let val (junk,newpctxt) = pop_absyn_stack pctxt
			in
			    (pop_block newpctxt (ABSYN.BLOCK(e::b)))
			end
		      | _ => (pctxt, ABSYN.BLOCK(b))
		end;
	    val (newctxt, b) = (pop_block ctxt (ABSYN.BLOCK([])))
	in
	    push_absyn_stack (ABSYN.BBLOCK(b)) newctxt
	end;

    (**************************************************************)
    (* Consume Action : hands actions off to appropriate handlers *)
    (**************************************************************)
    fun consumeAction action ctxt =
    let val (r,t,kv) = action
    in
        case t of
            "int-literal-constant" =>
            (int_literal_constant_handler kv ctxt)
          | "real-literal-constant" =>
            (real_literal_constant_handler kv ctxt)
          | "generic-name-list__begin" => (generic_name_list_begin [] ctxt)
          | "generic-name-list-part" => (generic_name_list_part kv ctxt)
          | "generic-name-list" => (generic_name_list kv ctxt)
          | "part-ref" => (part_ref kv ctxt)
          | "data-ref" => (data_ref kv ctxt)
          | "language-binding-spec" => (language_binding_spec kv ctxt)
          | "prefix-spec" => (prefix_spec kv ctxt)
          | "prefix" => (prefix kv ctxt)
          | "name" => (name kv ctxt)
	  | "suffix" => (suffix kv ctxt)
          | "function-stmt" => (function_stmt kv ctxt)
          | "end-function-stmt" => (end_function_stmt kv ctxt)
          | "ext-function-subprogram" => (ext_function_subprogram kv ctxt)
          | "intrinsic-type-spec" => (intrinsic_type_spec kv ctxt)
          | "entity-decl-list__begin" => (entity_decl_list_begin kv ctxt)
          | "entity-decl" => (entity_decl kv ctxt)
          | "entity-decl-list" => (entity_decl_list kv ctxt)
          | "type-declaration-stmt" => (type_declaration_stmt kv ctxt)
          | "main-program__begin" => (main_program_begin kv ctxt)

(* module support *)
          | "module" => (module kv ctxt)
          | "module-stmt" => (module_stmt kv ctxt)
          | "module-subprogram" => (module_subprogram kv ctxt)
          | "module-subprogram-part" => (module_subprogram_part kv ctxt)
          | "end-module-stmt" => (end_module_stmt kv ctxt)

          | "variable" => (variable kv ctxt)
          | "add-operand__add-op" => (add_operand_add_op kv ctxt)
          | "add-operand" => (add_operand kv ctxt)
          | "assignment-stmt" => (assignment_stmt kv ctxt)
          | "mult-operand__mult-op" => (mult_operand_mult_op kv ctxt)
          | "end-program-stmt" => (end_program_stmt kv ctxt)
          | "program-stmt" => (program_stmt kv ctxt)
          | "do-stmt" => (do_stmt kv ctxt)
          | "end-do-stmt" => (end_do_stmt kv ctxt)
	  | "loop-control" => (loop_control kv ctxt)
	  | "do-variable" => (do_variable kv ctxt)
          | "level-3-expr" => (level_3_expr kv ctxt)
          | "if-then-stmt" => (if_then_stmt kv ctxt)
          | "else-stmt" => (else_stmt kv ctxt)
          | "else-if-stmt" => (else_if_stmt kv ctxt)
          | "end-if-stmt" => (end_if_stmt kv ctxt)
          | "section-subscript" => (section_subscript kv ctxt)
          | "section-subscript-list__begin" => 
	    (section_subscript_list_begin kv ctxt)
	  | "initialization" => (initialization kv ctxt)
	  | "dummy-arg-list__begin" => (dummy_arg_list_begin kv ctxt)
	  | "dummy-arg" => (dummy_arg kv ctxt)
	  | "subroutine-stmt" => (subroutine_stmt kv ctxt)
	  | "end-subroutine-stmt" => (end_subroutine_stmt kv ctxt)
	  | "logical-literal-constant" => (logical_literal_constant kv ctxt)
	  | "cycle-stmt" => (cycle_stmt kv ctxt)
          | "array-spec__begin" => (array_spec_begin kv ctxt)
          | "array-spec-element" => (array_spec_element kv ctxt)
          | "array-spec" => (array_spec kv ctxt)
	  | "intent-spec" => (intent_spec kv ctxt)
          | "kind-selector" => (kind_selector kv ctxt)
	  | "declaration-type-spec" => (declaration_type_spec kv ctxt)
          | "attr-spec" => (attr_spec kv ctxt)
          | "t-prefix-spec" => (t_prefix_spec kv ctxt)
          | "t-prefix" => (prefix kv ctxt)
	  | "call-stmt" => (call_stmt kv ctxt)
	  | "power-operand" => (power_operand kv ctxt)
	  | "where-stmt" => (where_stmt kv ctxt)
	  | "where-construct-stmt" => (where_construct_stmt kv ctxt)
	  | "where-body-construct" => (where_body_construct kv ctxt)
	  | "end-where-stmt" => (end_where_stmt kv ctxt)
	  | "elsewhere-stmt" => (elsewhere_stmt kv ctxt)
	  | "elsewhere-stmt__end" => (elsewhere_stmt_end kv ctxt)
	  | "output-item" => (output_item kv ctxt)			   
	  | "print-stmt" => (print_stmt kv ctxt)
	  | "char-literal-constant" => (char_literal_constant kv ctxt)
	  | "executable-construct" => (executable_construct kv ctxt)
	  | "block" => (block_handler kv ctxt)

	  (* IN PROGRESS *)
	  | "implicit-stmt" => (implicit_stmt kv ctxt)
	  | "implicit-spec-list__begin" => ((warn t); ctxt)
	  | "implicit-spec-list" => ((warn t); ctxt)
	  | "implicit-spec" => (implicit_spec kv ctxt)

	  | "actual-arg" => ((warn t); ctxt)
	  | "actual-arg-spec" => (actual_arg_spec kv ctxt)

	  | "format" => ((warn t);ctxt)
	  | "output-item-list" => ((warn t);ctxt)

	  | "letter-spec-list__begin" => ((ignore t); ctxt)
	  | "letter-spec" => (letter_spec kv ctxt)
	  | "continue-stmt" => (continue_stmt kv ctxt)
	  | "label" => (handle_label kv ctxt)

	  | "letter-spec-list" => ((ignore t); ctxt)

	  (* INTENTIONALLY IGNORED *)
	  | "where-stmt__begin" => ((ignore t);ctxt)
	  | "where-construct" => ((ignore t);ctxt)
	  | "declaration-construct" => ((ignore t);ctxt)
	  | "primary" => ((ignore t);ctxt)
	  | "literal-constant" => ((ignore t);ctxt)
	  | "kind-param" => ((ignore t);ctxt)
          | "main-program" => ((ignore t);ctxt)
          | "module-stmt__begin" => ((ignore t);ctxt)
          | "designator" => ((ignore t);ctxt)
          | "type-declaration-stmt__begin" => ((ignore t);ctxt)
          | "specification-part" => ((ignore t);ctxt)
          | "mult-operand" => ((ignore t);ctxt)
	  | "mult-op" => ((ignore t);ctxt)
	  | "power-op" => ((ignore t);ctxt)
	  | "dummy-arg-list" => ((ignore t);ctxt)
          | "section-subscript-list" => ((ignore t);ctxt)
	  | "subroutine-stmt__begin" => ((ignore t);ctxt)
	  | "function-stmt__begin" => ((ignore t);ctxt)
	  | "execution-part" => ((ignore t);ctxt)
	  | "execution-part-construct" => ((ignore t);ctxt)
	  | "action-stmt" => ((ignore t);ctxt)
	  | "rel-op" => ((ignore t); ctxt)
	  | "if-construct" => ((ignore t); ctxt)
	  | "output-item-list__begin" => ((ignore t); ctxt)
	  | "result-name" => ((ignore t); ctxt)
          | "proc-language-binding-spec" => ((ignore t); ctxt)
	  | "start-of-file" => ((ignore t); ctxt)
	  | "end-of-file" => ((ignore t); ctxt)
	  | "end-do" => ((ignore t); ctxt) (* R833 *)

          (* BELOW HERE : NOT DONE *)
	  | "interface-stmt__begin" => ((warn t); ctxt)
	  | "interface-stmt" => ((warn t); ctxt)
	  | "interface-body" => ((warn t); ctxt)
	  | "interface-specification" => ((warn t); ctxt)
	  | "end-interface-stmt" => ((warn t); ctxt)
	  | "interface-block" => ((warn t); ctxt)
	  | "function-subprogram" => ((warn t); ctxt)
	  | "block-do-construct" => ((warn t); ctxt)
	  | "do-construct" => ((warn t); ctxt)
	  | "stop-stmt" => ((warn t); ctxt)
	  | "procedure-designator" => ((warn t); ctxt)
	  | "add-op" => ((ignore t);ctxt) (* ??? *)
	  | "scalar-int-variable" => ((warn t);ctxt)
	  | "designator-or-func-ref" => ((ignore t);ctxt)
	  | "named-constant-def-list__begin" => ((warn t);ctxt)
	  | "do-term-action-stmt" => ((warn t);ctxt)
	  | "return-stmt" => ((warn t);ctxt)
	  | "parameter-stmt" => ((warn t);ctxt)
	  | "named-constant-def-list" => ((warn t);ctxt)
      | "end-of-stmt" => ((warn t);ctxt)
      | "derived-type-stmt" => ((warn t);ctxt)
      | "component-decl-list__begin" => ((warn t);ctxt)
      | "component-decl" => ((warn t);ctxt)
      | "component-decl-list" => ((warn t);ctxt)
      | "data-component-def-stmt" => ((warn t);ctxt)
      | "component-def-stmt" => ((warn t);ctxt)
      | "end-type-stmt" => ((warn t);ctxt)
	  | "internal-subprogram" => ((warn t);ctxt)
	  | "internal-subprogram-part" => ((warn t);ctxt)
      | "type-param-or-comp-def-stmt-list" => ((fatal t);ctxt)
      | _ => ((fatal t);
              raise ActionHandlerException("UNSUPPORTED ACTION"))
    end;

end;

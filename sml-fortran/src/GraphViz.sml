(* 
 * code to dump ASTs and other structures to graphviz output 
 *
 * matt@galois.com
 *)
structure GraphViz =
struct
    structure ABSYN = AbstractSyntax;

    fun prr pf s id = pf(Int.toString(id)^" [label=\""^s^"\"];\n");
    fun prredge pf i1 i2 = pf(Int.toString(i1)^"->"^
			      Int.toString(i2)^";\n");
    fun lprredge pf i1 i2 s = pf(Int.toString(i1)^"->"^
				Int.toString(i2)^" [label=\""^s^"\"];\n");

    exception GVizException of string;

    fun primtypeString t =
	case t of
	    ABSYN.INTEGER => "INTEGER"
	  | ABSYN.REAL => "REAL"
	  | ABSYN.LOGICAL => "LOGICAL"
	  | ABSYN.DOUBLEPRECISION => "DOUBLE PRECISION"
	  | ABSYN.DOUBLECOMPLEX => "DOUBLE COMPLEX"
	  | ABSYN.COMPLEX => "COMPLEX"
	  | ABSYN.CHARACTER => "CHARACTER";

    fun printKindSelector pfunc ks id printer =
	let fun pr s id = (prr pfunc s id);
	    fun predge i1 i2 = (prredge pfunc i1 i2)
	in
	    case ks of
		ABSYN.LITERAL(i) => (pr ("KIND:"^Int.toString(i)) id;
				     (id+1))
	      | ABSYN.IDENT(s) => (pr ("KIND:"^s) id;
				   (id+1))
	      | ABSYN.KINDEXPR(e) => (printer pfunc e id)
	end;

    fun printType pfunc t id printer =
	let fun pr s id = (prr pfunc s id);
	    fun predge i1 i2 = (prredge pfunc i1 i2)
	    fun lpredge i1 i2 s = (lprredge pfunc i1 i2 s)
	in
	    case t of
		ABSYN.PRIMITIVE(pt,ks) => 
		  (pr (primtypeString pt) id;
		   case ks of
		       SOME(k) => (lpredge id (id+1) "KIND";
				   printKindSelector pfunc k (id+1) printer)
		     | NONE => (id+1)
		  )
	      | _ => (pr "Unsupported type" id;
		      (id+1))
	end;

    fun printASpecs pfunc [] id gpid printer = id
      | printASpecs pfunc (asp::asps) id gpid printer =
	let fun pr s id = (prr pfunc s id);
	    fun predge i1 i2 = (prredge pfunc i1 i2)
	in
	    printASpecs pfunc asps 
			(predge gpid id;
			 case asp of
			     ABSYN.AS_EXPR(e) => (printer pfunc e id)
			   | ABSYN.AS_EXPR_COLON(e) => (printer pfunc e id)
			   | ABSYN.AS_EXPR_COLON_EXPR(e1,e2) =>
			     let val sid = (printer pfunc e1 id)
			     in
				 predge gpid sid;
				 printer pfunc e2 sid
			     end
			   | ABSYN.AS_EXPR_COLON_ASTERISK(e) => (printer pfunc e id)
			   | ABSYN.AS_ASTERISK => (pr "*" id; (id+1))
			   | ABSYN.AS_COLON => (pr ":" id; (id+1))
			) gpid printer
	end;

    fun printEntities pfunc [] pid gpid printer = pid
      | printEntities pfunc (e::es) pid gpid printer =
	let fun pr s id = (prr pfunc s id);
	    fun predge i1 i2 = (prredge pfunc i1 i2);
	    val ABSYN.ENTITY({name=n, arrayspec=aspec,initializer=ini}) = e
	in
	    printEntities pfunc es
			  ( predge gpid pid;
			    pr ("ENTITY: "^n) pid;
			    let val iniid = case ini of
						SOME(e) => 
						(predge pid (pid+1);
						 pr ("INITIALIZER") (pid+1);
						 predge (pid+1) (pid+2);
						 printer pfunc e (pid+2))
					      | _ => (pid+1)
			    in
				case aspec of
				    SOME(ABSYN.ARRAYSPEC(aspecs)) => 
				    (predge pid iniid;
				     pr ("ARRAYSPEC") iniid;
				     printASpecs pfunc aspecs (iniid+1) iniid printer)
				  | _ => iniid
			    end
			  ) gpid printer
	end;	

    fun printAttrs pfunc [] pid gpid printer = pid
      | printAttrs pfunc (att::atts) pid gpid printer =
	let fun pr s id = (prr pfunc s id);
	    fun predge i1 i2 = (prredge pfunc i1 i2);
	in
	    printAttrs pfunc atts
		       ( predge gpid pid;
			 case att of
			    ABSYN.ATTR_ALLOCATABLE => 
			    (pr "ALLOCATABLE" pid;
			     (pid+1))
			  | ABSYN.ATTR_ASYNCHRONOUS => 
			    (pr "ASYNCHRONOUS" pid;
			     (pid+1))
			  | ABSYN.ATTR_DIMENSION(aspecs) => 
			    (pr "DIMENSION" pid;
			     printASpecs pfunc aspecs (pid+1) pid printer)
			  | ABSYN.ATTR_EXTERNAL =>
			    (pr "EXTERNAL" pid;
			     (pid+1))
			  | ABSYN.ATTR_INTENT(ispec) =>
			    (let val s = case ispec of
					     ABSYN.INTENT_IN => "INTENT(IN)"
					   | ABSYN.INTENT_OUT => "INTENT(OUT)"
					   | ABSYN.INTENT_INOUT =>
					     "INTENT(INOUT)"
			     in pr s pid end;
			     (pid+1))
			  | ABSYN.ATTR_PARAMETER =>
			    (pr "PARAMETER" pid;
			     (pid+1))
			  | _ => raise GVizException("UNIMPLEMENTED")
		       ) gpid printer
	end;	

    fun printer pfunc node id =
	let fun pr s id = (prr pfunc s id);
	    fun predge i1 i2 = (prredge pfunc i1 i2);
	    fun lpredge i1 i2 s = (lprredge pfunc i1 i2 s)
	in
	case node of
	    ABSYN.INTCONST(i) => (pr("INTCONST: "^Int.toString(#value i)) id; 
				  (id+1))
	  | ABSYN.REALCONST(r) => (pr("REALCONST: "^ (#value r)) id; 
				   (id+1))
	  | ABSYN.STRINGCONST(s) => (pr("STRINGCONST: "^s) id;
				     (id+1))
	  | ABSYN.CHARCONST(c) => (pr("CHARCONST: '"^str(c)^"'") id;
				   (id+1))
          | ABSYN.LOGICALCONST(l) => (let val v = (#value l)
	    			         val vs = case v of
                                                      true => "true"
                                                    | false => "false"
 				      in (pr("LOGICALCONST: "^vs) id;
				          (id+1))
			             end)
	  | ABSYN.VARIABLE(n) => 
	    ( pr ("VARIABLE") id;
	      predge id (id+1);
	      printer pfunc n (id+1) )
	  | ABSYN.DECLARATION({ty=t,attributes=atts,entities=es}) =>
	    ( let val tid = printType pfunc t (id+1) printer;
		  val aid = printAttrs pfunc atts tid id printer;
		  val eid = printEntities pfunc es aid id printer
	      in
		  pr "DECLARATION" id;
		  predge id (id+1);
		  eid
	      end
	    )
	  | ABSYN.BINARYOP({left=l,operand=binop,right=r}) =>
             (	
	      (case binop of
		   ABSYN.PLUS => (pr ("BINOP (+)") id)
		 | ABSYN.MINUS => (pr ("BINOP (-)") id)
		 | ABSYN.TIMES => (pr ("BINOP (*)") id)
		 | ABSYN.DIVIDE => (pr ("BINOP (/)") id)
		 | ABSYN.POWER => (pr ("BINOP (**)") id)
		 | ABSYN.LOGIC_AND => (pr ("BINOP (AND)") id)
		 | ABSYN.LOGIC_OR => (pr ("BINOP (OR)") id)
		 | ABSYN.LOGIC_EQV => (pr ("BINOP (EQV)") id)
		 | ABSYN.LOGIC_NEQV => (pr ("BINOP (NEQV)") id)
	      );
	      let val idl = (id+1)
		  val idr = (printer pfunc l idl)
	      in
		  predge id idl;
		  predge id idr;
		  printer pfunc r idr
	      end)
	  | ABSYN.RELOP({left=l,operand=relop,right=r}) =>
             (	
	      (case relop of
		   ABSYN.EQUALS => (pr ("RELOP (==)") id)
		 | ABSYN.NOTEQUALS => (pr ("RELOP (/=)") id)
		 | ABSYN.LESSTHAN => (pr ("RELOP (<)") id)
		 | ABSYN.LESSEQUAL => (pr ("RELOP (<=)") id)
		 | ABSYN.GREATERTHAN => (pr ("RELOP (>)") id)
		 | ABSYN.GREATEREQUAL => (pr ("RELOP (>=)") id));
	      let val idl = (id+1)
		  val idr = (printer pfunc l idl)
	      in
		  predge id idl;
		  predge id idr;
		  printer pfunc r idr
	      end)
	  | ABSYN.PRINT(outputitems) =>
	    (
	     let fun oiprint [] oid = oid
		   | oiprint (i::ii) oid =
		     let val onewid = printer pfunc i oid
		     in (predge id oid;
			 oiprint ii onewid)
		     end
	     in
		 pr ("PRINT") id;
		 oiprint outputitems (id+1)
	     end
	    )
	  | ABSYN.PROGSTMT({name=n, scope=s, body=ABSYN.BLOCK(b)}) =>
 	    (let fun bprint [] bid = bid
		   | bprint (s::ss) bid = 
		     let val bnewid = printer pfunc s bid
		     in (predge id bid;
			 bprint ss bnewid)
		     end
	     in
		 (case n of
		      SOME(name) => pr ("PROGRAM: "^name) id
		    | NONE => pr ("PROGRAM: *NO NAME*") id);
		 bprint b (id+1)
	     end
	    )
	  | ABSYN.FUNCSTMT({name=n,
                            attrs=at, body=ABSYN.BLOCK(b),args=a,scope=s}) =>
 	    (let fun bprint [] bid = bid
		   | bprint (s::ss) bid = 
		     let val bnewid = printer pfunc s bid
		     in (predge id bid;
			 bprint ss bnewid)
		     end;
		 fun aprint [] arid aid = aid
		   | aprint (a::aa) arid aid =
			 (pr a aid;
			  predge arid aid;
			  aprint aa arid (aid+1));
		 fun printargs aid =
		     (pr ("ARGS") aid;
		      predge id aid;
		      aprint a aid (aid+1))
	     in
		 pr ("FUNCTION: "^n) id;
		 bprint b (printargs (id+1))
	     end
	    )	    
	  | ABSYN.SUBROUTINE({name=n, attrs=at, scope=s, body=ABSYN.BLOCK(b),args=a}) =>
 	    (let fun bprint [] bid = bid
		   | bprint (s::ss) bid = 
		     let val bnewid = printer pfunc s bid
		     in (predge id bid;
			 bprint ss bnewid)
		     end;
		 fun aprint [] arid aid = aid
		   | aprint (a::aa) arid aid =
			 (pr a aid;
			  predge arid aid;
			  aprint aa arid (aid+1));
		 fun printargs aid =
		     (pr ("ARGS") aid;
		      predge id aid;
		      aprint a aid (aid+1))
	     in
		 pr ("SUBROUTINE: "^n) id;
		 bprint b (printargs (id+1))
	     end
	    )	    
	  | ABSYN.IFTHENELSE({closed=junk,cond=c,
			      else_clause=ec,then_clause=tc,...}) =>
	    ( pr "IFTHENELSE" id;
	      predge id (id+1);
	      let fun bprint [] pid bid = bid
		    | bprint (s::ss) pid bid = 
		      let val bnewid = printer pfunc s bid
		      in (predge pid bid;
			  bprint ss pid bnewid)
		      end
		  val cid = printer pfunc c (id+1)
		  val tid = case tc of
				SOME(ABSYN.BLOCK(t)) => 
				(pr ("THEN") cid;
				 predge id cid;
				 bprint t cid (cid+1))
			      | NONE => cid;
		  val eid = case ec of
				SOME(ABSYN.BLOCK(e)) => 
				(pr ("ELSE") tid;
				 predge id tid;
				 bprint e tid (tid+1))
			      | NONE => tid
	      in
		  eid
	      end
	     
	    )
	  | ABSYN.WHERE({cond=c,body=ABSYN.BLOCK(b),
			 else_clause=e, ...}) => 
 	    (let fun bprint [] pid bid = bid
		   | bprint (s::ss) pid bid = 
		     let val bnewid = printer pfunc s bid
		     in (predge pid bid;
			 bprint ss pid bnewid)
		     end
	     in
		 ( pr "WHERE" id;
		   lpredge id (id+1) "COND";
		   let val cid = (printer pfunc c (id+1))
		       val bid = bprint b cid (cid+1)
		   in
		       pr "BODY" cid;
		       predge id cid;
		       case e of
			   SOME(ABSYN.BLOCK(eb)) =>
			   ( pr "ELSEWHERE" bid;
			     predge id bid;
			     bprint eb bid (bid+1)
			   )
			 | _ => bid
		   end
		 )
	     end
	    )
	  | ABSYN.DOLOOP({ctrl=c,body=ABSYN.BLOCK(b),
			  label=lbl,closed=junk2}) =>
 	    (let fun bprint [] bid = bid
		   | bprint (s::ss) bid = 
		     let val bnewid = printer pfunc s bid
		     in (predge id bid;
			 bprint ss bnewid)
		     end
	     in
		 ( case c of
		       SOME(ABSYN.WHILE(t)) => 
		       ( pr "DOLOOP WHILE" id;
			 predge id (id+1);
			 bprint b (printer pfunc t (id+1)) )
		     | SOME(ABSYN.ITERATE({var=v,lo=l,hi=h,inc=i})) =>
		       ( pr "DOLOOP" id;
			 let val vid = printer pfunc v (id+1)
			     val lid = printer pfunc l vid
			     val hid = printer pfunc h lid
			     val labid = case lbl of
					     SOME(s) => (pr("LABEL:"^s) hid;
							 (hid+1))
					   | NONE => hid
			     val iid = case i of 
					   SOME(ii) => printer pfunc ii labid
					 | NONE => labid
			 in
			     ( lpredge id (id+1) "VAR";
			       lpredge id vid "LO";
			       lpredge id lid "HI";
			       (case lbl of
				   SOME(s) => lpredge id hid "LABEL"
				 | _ => ()
			       );
			       (if iid=labid then ()
				else lpredge id labid "INCR");
			       bprint b iid)
			 end
		       )
		     | _ => ( pr "DOLOOP" id;
			      bprint b (id+1) )
		 )
	     end
	    )
	  | ABSYN.UNARYOP({node=n,operand=unop}) =>
             (	
	      (case unop of
		   ABSYN.UMINUS => (pr ("UNARYOP (-)") id)
		 | ABSYN.UPLUS => (pr ("UNARYOP (+)") id)
		 | ABSYN.NOT => (pr ("UNARYOP (not)") id));
	      predge id (id+1);
	      printer pfunc n (id+1) )
	  | ABSYN.CALLSTMT(n) =>
	    (pr "CALL" id;
	     predge id (id+1);
	     printer pfunc n (id+1) )
	  | ABSYN.EXECCONS(e) =>
	    (printer pfunc e id)
	  | ABSYN.ASSIGN({lhs=l,rhs=r}) =>
	    ( (pr ("ASSIGN (=)") id);
	      let val idl = (id+1)
		  val idr = (printer pfunc l idl)
	      in
		  predge id idl;
		  predge id idr;
		  printer pfunc r idr
	      end )
	  | ABSYN.SUBSCRIPTLIST(ssl) =>
	    ( let fun printssl dimid [] sid = sid
		    | printssl dimid ({lo=lo, hi=hi, stride=stride}::ss) sid =
		      (let val lid = (case lo of
					  SOME(i) => (printer pfunc i (sid+1))
					| NONE => (sid+1));
			   val hid = (case hi of
					  SOME(i) => (printer pfunc i lid)
					| NONE => lid);
			   val stid = (case stride of
					   SOME(i) => (printer pfunc i hid)
					 | NONE => hid)
		       in
			   predge id sid;
			   if lid <> (sid+1) then 
			       (lpredge sid (sid+1) "LO")
			   else ();
			   if hid <> lid then
			       (lpredge sid lid "HI")
			   else ();
			   if stid <> hid then
			       (lpredge sid hid "STRIDE")
			   else ();
			   pr ("DIM "^Int.toString(dimid)) sid;
			   printssl (dimid+1) ss stid
		       end)
	      in
		  pr ("SUBSCRIPT") id;
		  printssl 1 ssl (id+1)
	      end )
	  | ABSYN.CYCLE({name=n}) =>
	    ( pr ("CYCLE") id;
	      (id+1))
	  | ABSYN.DATAREF(prl:ABSYN.PartRef list) =>
	    ( (pr ("DATAREF") id);
	      let fun pprint [] pid = pid
		    | pprint ((ABSYN.PARTREF(s)::ss):ABSYN.PartRef list) pid = 
		      let val pnewid = pid+1
		      in (pr ("PARTREF: "^(#id (#token s))) pid;
			  predge id pid;
			  pprint ss 
				 (case (#selectionSubscriptList s) of
				      SOME(node) => (predge pid pnewid;
						     printer pfunc node pnewid)
				    | _ => pnewid))
		      end
	      in
		  (pprint prl (id+1))
	      end
	    )
	  | ABSYN.IMPLICIT_STMT(ABSYN.IMPLICIT_NONE) =>
	    (pr ( "IMPLICIT NONE" ) id; (id + 1))
	  | _ => (pr ( "**UNSUPPORTED**") id; (id+1))
	end;
end;

(*
 * Driver.sml
 *
 * These are functions either called as main() for a standalone executable
 * or as convenience functions to invoke the ActionHandler iteratively in
 * interactive mode.
 *
 * matt@galois.com
 *)
signature DRIVER =
sig
    type ACTION;

    val openfile: string -> TextIO.instream;
    val processor: TextIO.instream -> 
		   (string * string * (string * ActionParse.Action) list) list;
    val callhandlers : (string * string * 
			(string * ActionParse.Action) list) list ->
                       ActionHandlers.context -> ActionHandlers.context;
    val doone : (string * string * (string * ActionParse.Action) list) list ->
                ActionHandlers.context ->
                ( (string * string * (string * ActionParse.Action) list) list *
                  ActionHandlers.context );
    val setup : string -> 
                ( (string * string * (string * ActionParse.Action) list) list *
                  ActionHandlers.context );

    val roundtrip : string -> string -> unit;

    val unparse : string -> unit;
    val typecons : string -> unit;

    val main : string * string list -> int;
end;
	  
structure Driver :> DRIVER =
struct
    type ACTION = ActionParse.Action;

    fun openfile fname = TextIO.openIn fname;

    fun chop_newline s = 
	if (String.size s)=1 then ""
	else String.extract(s,0,SOME ((String.size s)-1) );
	 
    fun processor istr =
	let val ll = (TextIO.inputLine istr)
	in
	    case ll of 
		SOME(l) => 
  		let 
		    val line = chop_newline l
		in 
		    if ((String.size line) > 0) then
			((ActionParse.decompose_action line))::(processor istr)
		    else
			(processor istr)
		end
	      | _ => []
	end;

    fun callhandlers [] ctxt = ctxt
      | callhandlers (c::cs) ctxt =
        ( callhandlers (cs) (ActionHandlers.consumeAction (c) ctxt) );

    fun doone [] ctxt = ( [], ctxt )
      | doone (c::cs) ctxt = ( cs, (ActionHandlers.consumeAction (c) ctxt));

    fun setup fname =
	let val f = openfile fname;
	    val p = processor f;
	    val ctxt = ActionHandlers.empty_context ()
	in
	    (p,ctxt)
	end;

    fun roundtrip fname outfname =
	let val (p,ctxt) = setup fname;
	    val ctxt = callhandlers p ctxt;
	    val outhandle = TextIO.openOut outfname;
	    fun pfunc s = (TextIO.output (outhandle, s); 
			   TextIO.flushOut outhandle);
	    fun pr [] n = ()
	      | pr (s::ss) n = (pr ss (GraphViz.printer pfunc s n))
	in
	    (pfunc "digraph {\n";
	     pr (#absyn_stack ctxt) 1;
	     pfunc "}\n")
	end;

    fun unparse fname =
	let val (p,ctxt) = setup fname;
	    val ctxt = callhandlers p ctxt;
	    val outhandle = TextIO.stdOut;
	    fun pfunc s = (TextIO.output (outhandle, s); 
			   TextIO.flushOut outhandle);
	    fun unp [] pin = ()
	      | unp (s::ss) pin = (unp ss (Unparse.unparse s pin))
            val pinfo = Unparse.create_pInfo pfunc "  " 0 0;
	in
	    unp (#absyn_stack ctxt) pinfo
	end;

    fun typecons fname =
	let val (p,ctxt) = setup fname;
	    val ctxt = callhandlers p ctxt;
	    val outhandle = TextIO.stdOut;
	    fun pfunc s = (TextIO.output (outhandle, s); 
			   TextIO.flushOut outhandle);
	    fun vis func [] en = ()
	      | vis func (s::ss) en = (vis func ss (Visit.visit func s en))
            val env = Visit.create_env (#c_scope ctxt)
	in
	    vis TypeCon.check (#absyn_stack ctxt) env
	end;

    exception BadArgumentException;

    fun get_argument [] aval errstr = (TextIO.print(errstr^"\n");
				       OS.Process.exit 1;
				       (NONE,[]))
      | get_argument (arg::args) aval errstr =
	let fun nextarg [] = (TextIO.print(errstr^"\n");
			      OS.Process.exit 1;
			      (NONE,[]))
	      | nextarg (x::xs) = (SOME(x),xs)
	in
	    if (String.compare(aval,arg) = EQUAL) then
		nextarg args
	    else
		get_argument args aval errstr
	end;

    fun main (prog:string,args) =
	let val (opt_infile,rargs) = 
		get_argument args "-i" "No input specified.";
	    val (opt_outfile,rargs) = 
		get_argument rargs "-o" "No output specified.";
	    val infile = case opt_infile of
			     NONE => raise BadArgumentException
			   | SOME(s) => s;
	    val outfile = case opt_outfile of
			      NONE => raise BadArgumentException
			    | SOME(s) => s
	in
	    ( roundtrip infile outfile;
	      1)
	end;
end;

(********************************************************************)
(* ActionParse.sml                                                  *)
(*                                                                  *)
(* This structure is responsible for taking in the string output    *)
(* from the parser and turning it into structures that the action   *)
(* handler can do something useful with.                            *)
(*                                                                  *)
(* matt@galois.com                                                  *)
(********************************************************************)
structure ActionParse =
struct

    structure TK = Tokens;

    (* a token has an ID string, a line and column number *)
    type token_type = {id:string,
		       ty:TK.TOKEN_TYPE,
		       line:int,
		       col:int};

    (* when possible, we translate the value of an action key-value pair
       into a type that has some meaning.  these are the meaningful value
       types. *)
    datatype ACTION_VALUE = STRING of string
			  | BOOL of bool
			  | INT of int
			  | TOKEN of token_type;

    (* renaming the type for convenience *)
    type Action = ACTION_VALUE;

    (* string processing : given a string that has been exploded into a list
       of characters and a separator, return a tuple containing the string up
       to the separator and the remaining characters.  See the String structure
       for the explode function that yields the list of characters. *)
    fun poptochar sep [] = ("",[])
      | poptochar sep (c::cs) =
	if (c = sep) then ("",cs)
	else let val (s,remainder) = (poptochar sep cs)
	     in 
		 ( ((String.str c)^(s)), remainder)
	     end;

    (* A useful exception *)
    exception ActionParseException of string;

    (* the extract functions take ACTION_VALUE options and de-optionize them 
       into the type values. *)
    fun extract_token t =
	case t of
	    SOME(TOKEN(tt)) => tt
	  | _ => raise ActionParseException("Bad Token");

    fun extract_bool b =
	case b of
	    SOME(BOOL(bb)) => bb
	  | _ => raise ActionParseException("Bad Bool");

    fun extract_real r =
	case r of
	    SOME(STRING(rr)) => rr
	  | SOME(INT(i)) => (Int.toString(i)^".0")
	  | _ => raise ActionParseException("Bad Real");

    fun extract_string s =
	case s of
	    SOME(STRING(ss)) => ss
	  | _ => raise ActionParseException("Bad String");
	
    fun extract_int i =
	case i of
	    SOME(INT(ii)) => ii
	  | _ => raise ActionParseException("Bad Int");

    (* spin past whitespace in a character list *)
    fun skipwhitespace [] = []
      | skipwhitespace (c::cs) =
	if (c = #" ") then (skipwhitespace cs)
	else (c::cs);
	
    (* given a character sequence of key-value pairs formatted as

       key=value key=value key=value

       this will return a tuple of the next available key, value, and the
       remaining list of characters *)
    fun popkeyvalue [] = ("","",[])
      | popkeyvalue clist =
	let val clist2 = skipwhitespace clist;
	    val (key, krem) = (poptochar (#"=") clist2);
	    val (value, vrem) = (poptochar (#" ") krem)
	in
	    (key, value, vrem)
	end;

    (* turn a list of strings into a list of integers if possible *)
    fun strlist_to_ints [] = []
      | strlist_to_ints (s::ss) =
	let val i = Int.fromString(s)
	in
	    case i of
		NONE => (strlist_to_ints ss)
	      | SOME(x) => (x)::(strlist_to_ints ss)
	end;

    (* convenience wrapper around String.tokens for later conciseness *)
    fun tokenize c s = 
	(String.tokens (fn cc => if (cc = c) then true else false)
		       s);
	
    (* given a character c and a list of characters, give the list of characters
       that occur up to the first occurrance of c *)
    fun upto c s [] = (s,[])
      | upto c s (x::xs) = if (x = c) then (s,xs)
			   else (upto c (s@[x]) xs);

    (* turn a string representation of a token into the token type defined 
       above *)
    fun parseToken s =
	case s of
	    (* gross hack to deal with programs like this
a = x(3)
end
               in this case, the id on the program statement is null with a
	       carriage return...
             *)
	    "null" => {id="null",ty=TK.T_UNKNOWN,line=0,col=0}
	  | _ =>
	    let val mid = String.extract(s,1,SOME ((String.size s)-2));
		val (junk,remainder) = (upto #"=" [] (String.explode(mid)));
		val (tokval,remainder) = 
		    case (hd(tl(remainder))) of
			#"'" => let val (tv,rm) = (upto #"'" [] 
							(tl(tl(remainder))))
				in
				    (([#"'"]@tv)@[#"'"], (tl(rm)))
				end
		      | _ => (upto #"'" [] (tl(remainder)));
		val (tmp,location) = upto #"," [] (tl(remainder));
                val (junk,tmp) = (upto #"<" [] tmp);
                val (tmp,junk) = (upto #">" [] tmp);
                val tmp = Int.fromString (String.implode tmp);
                val ttype = (case tmp of
                                 SOME(i) => TK.enumToTokenType i
                               | _ => raise ActionParseException("Bad Value"))
		val name = ("'"^(String.implode tokval)^"'");
		val loc = strlist_to_ints(tokenize #":" 
						   (String.implode location))
	    in
		{id=name,ty=ttype,line=hd(loc),col=hd(tl(loc))}
	    end;
	
    (* try to infer the actual type of a value instead of leaving everything
       as plain old strings *)
    fun toActionValue s =
	let val intVal = Int.fromString(s);
	    val realVal = Real.fromString(s)
	in
	    case s of
		"false" => BOOL(false)
	      | "true" => BOOL(true)
	      | _ => (case intVal of
			  SOME(i) => 
			  (case realVal of
			       SOME(r) => if Real.floor(r) = Real.ceil(r) then
					      INT(i)
					  else
					      STRING(s)
			     | _ => STRING(s))
			| _ => STRING(s))
	end;
	
    (* turn a string representation of a token into something with an
       ACTION_VALUE value *)
    fun tokenValue tok =
	let val {id=v,ty=t,line=l,col=c} = parseToken(tok);
	    val vv = String.extract(v,1,SOME ((String.size v)-2))
	in
	    (toActionValue vv)
	end;

    (* call string processing to decompose an action string into something more
	    useful *)
    fun decompose_action s =
	let val (rule, rem1) = (poptochar (#":") (String.explode s));
	    val (action, rem2) = (poptochar (#":") rem1);
	    fun ambiguousToken v =
		case v of 
		    "null" => (toActionValue v)
		  | _ => (TOKEN (parseToken v));
		
	    fun popkeys [] = []
	      | popkeys rlist =
		let val (k,v,r) = (popkeyvalue rlist)
		in
		    case k of
			"id" => (((k,(TOKEN (parseToken v))))::(popkeys r))
		      | "id1" => (((k,(TOKEN (parseToken v))))::(popkeys r))
		      | "lbl" => (((k,(TOKEN (parseToken v))))::(popkeys r))
		      | "str" => (((k,(TOKEN (parseToken v))))::(popkeys r))
		      | "id2" => (((k,(ambiguousToken v)))::(popkeys r))
		      | "name" => (((k,(TOKEN (parseToken v))))::(popkeys r))
	          | "dummy" => (((k,(TOKEN (parseToken v))))::(popkeys r))
		      | "relOp" => (((k,(TOKEN (parseToken v))))::(popkeys r))
		      | "digitString" => ((k,(tokenValue v))::(popkeys r))
		      | "realConstant" => ((k,(tokenValue v))::(popkeys r))
		      | "kindParam" => (((k,(TOKEN (parseToken v))))::(popkeys r))
		      | "spec" => (((k,(TOKEN (parseToken v))))::(popkeys r))
		      | "addOp" => (((k,(ambiguousToken v)))::(popkeys r))
		      | "multOp" => (((k,(ambiguousToken v)))::(popkeys r))
		      | _ =>  ((k,(toActionValue v))::(popkeys r))
		end;
	    val keyvalues = (popkeys rem2)
	in
	    (rule,action,keyvalues)
	end;
end;


(*
 * Symbol table
 *
 * Code taken from Appel's "Modern Compiler Implementation in ML" book, with
 * slight modifications and additions for the OFP project.
 * 
 * matt@galois.com
 *)
signature SYMBOL =
sig
    eqtype symbol
    val symbol : string -> symbol
    val name : symbol -> string
                         
    type 'a table
    val printsymtab : unit -> unit
    val printNumItems : 'a table -> unit
    val empty     : 'a table
    val enter     : 'a table * symbol * 'a -> 'a table
    val listenter : 'a table * string list * 'a -> 'a table
    val look      : 'a table * symbol -> 'a option
end;

structure SymbolTable :> SYMBOL =
struct
    type symbol = string * int

    exception Symbol
              
    val nextsym = ref 0
    val hashtable : (string, int) HashTable.hash_table =
        HashTable.mkTable(HashString.hashString, op =) (128,Symbol)

    fun symbol name =
        case HashTable.find hashtable name
         of SOME i => (name,i)
          | NONE => let val i = !nextsym
                    in nextsym := i+1;
		       TextIO.print("INSERTING "^name^" as "^Int.toString(i)^"\n");
                       HashTable.insert hashtable (name,i);
                       (name,i)
                    end

    fun name (s,n) = s

    fun printNumItems t =
        TextIO.print("SYMBOL TABLE: numItems=" ^ Int.toString(IntBinaryMap.numItems(t)) ^ ".\n")

    fun printsymtab () =
	let val symlist = HashTable.listItemsi hashtable
	    fun printsyms [] = TextIO.print("No more symbols.\n")
	      | printsyms ((k,v)::rest) = 
		(TextIO.print("("^Int.toString(v)^") "^k^"\n");
		 printsyms rest)
	in
	    printsyms symlist
	end;

    type 'a table = 'a IntBinaryMap.map

    val empty = IntBinaryMap.empty

    fun enter(t: 'a table, (s,n):symbol, a: 'a) = IntBinaryMap.insert(t,n,a)

    fun look(t: 'a table, (s,n): symbol) = IntBinaryMap.find(t,n)

    fun listenter(t: 'a table, []:      string list, a: 'a ) = t
      | listenter(t: 'a table, (n::ns): string list, a: 'a ) =
        let val sym = symbol n;
            val newt = enter (t,sym,a)
        in listenter(newt,ns,a) end;
end;

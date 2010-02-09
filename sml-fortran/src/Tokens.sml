structure Tokens =
struct
    datatype TOKEN_TYPE = T_DIGIT_STRING
	                | T_ELEMENTAL
	                | T_PURE
	                | T_RECURSIVE
	                | T_IDENT
	                | T_UNKNOWN;

    fun enumToTokenType i =
	case i of
	     10 => T_DIGIT_STRING
          |  92 => T_ELEMENTAL
          | 145 => T_PURE
          | 147 => T_RECURSIVE
	  | 206 => T_IDENT
	  | _   => T_UNKNOWN

end;
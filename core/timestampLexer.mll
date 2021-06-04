{
  open TimestampParser
}

let def_integer = (['1'-'9'] ['0'-'9']* | '0')
let def_float = (def_integer '.' ['0'-'9']+ ('e' ('-')? def_integer)?)

rule lex = parse
  | eof                { EOF }
  | def_integer as i   { UINTEGER (int_of_string i) }
  | def_float as f     { UFLOAT (float_of_string f) }
  | '+'                { PLUS }
  | '-'                { MINUS }
  | ':'                { COLON }
  | "infinity"         { INFINITY }

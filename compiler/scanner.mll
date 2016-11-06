{ open Parser }
let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']
let variable = letter (letter | digit | '_') *
rule token =
parse [' ' '\t' '\r' '\n'] { token lexbuf }
(* comment *)
| "/*" { comment lexbuf }
(* calculation *)
| '+' { PLUS }
| '-' { MINUS }
| '*' { TIMES }
| '/' { DIVIDE }
| '%' { MOD }
(* separator *)
| ';' { SEMICOLUMN }
| ',' { SEQUENCE }
| '=' { ASSIGN }
| ':' { COLUMN }
| '.' { DOT }
(* logical operation *)
| "and" { AND }
| "or" { OR }
| "not" { NOT }
| "if" { IF }
| "else" { ELSE }
| "for" { FOR }
| "break" { BREAK }
| "continue" { CONTINUE }
| "in" { IN }
| "return" {RETURN}
(* comparator *)
| '>' { GREATER }
| ">=" { GREATEREQUAL }
| '<' { SMALLER }
| "<=" { SMALLEREQUAL }
| "==" { EQUAL}
| "!=" { NOTEQUAL}
(* graph operator *)
| "--" { LINK }
| "->" { RIGHTLINK }
| "<-" { LEFTLINK }
| '@' { AT }
| '&' { AMPERSAND }
| '~' { SIMILARITY }
(* primary type *)
| "int" { INT }
| "float" { FLOAT }
| "string" { STRING }
| "bool" { BOOL }
| "node" { NODE }
| "graph" { GRAPH }
| "list" { LIST }
| "dict" { DICT }
| "null" { NULL }
(* integer and float *)
| digit+ as lit { INT_LITERAL(int_of_string lit) }
| digit+'.'digit+ as lit { FLOAT_LITERAL(float_of_string lit) }
| '"' (([^ '"'] | "\\\"")* as lit) '"' { STRING_LITERAL(lit) }
(* quote *)
| '"'  { QUOTE }
(* boolean operation *)
| "true" { TRUE }
| "false" { FALSE }
(* bracket *)
| '[' { LEFTBRACKET }
| ']' { RIGHTBRACKET }
| '{' { LEFTCURLYBRACKET }
| '}' { RIGHTCURLYBRACKET }
| '(' { LEFTROUNDBRACKET }
| ')' { RIGHTROUNDBRACKET }
(* id *)
| variable as id { ID(id) }
| eof { EOF }

and comment =
    parse "*/" {token lexbuf}
        | _ {comment lexbuf}

{
  open Parser
  let unescape s =
    	Scanf.sscanf ("\"" ^ s ^ "\"") "%S%!" (fun x -> x)
}
let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']
let variable = letter (letter | digit | '_') *
let escape = '\\' ['\\' ''' '"' 'n' 'r' 't']
let ascii = ([' '-'!' '#'-'[' ']'-'~'])
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
| "while" { WHILE}
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
| "void" { VOID }
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
| digit+'.'digit* as lit { FLOAT_LITERAL(float_of_string lit) }
| '"' ((ascii | escape)* as lit) '"' { STRING_LITERAL(unescape lit) }
(* quote *)
| '"'  { QUOTE }
(* boolean operation *)
| "true" | "false" as boollit { BOOL_LITERAL(bool_of_string boollit)}
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

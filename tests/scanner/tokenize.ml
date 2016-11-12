open Parser

let stringify = function
  (* calculation *)
  | PLUS -> "PLUS"   | MINUS -> "MINUS"
  | TIMES -> "TIMES" | DIVIDE -> "DIVIDE"
  | MOD -> "MOD"
  (* separator *)
  | SEMICOLUMN -> "SEMICOLUMN" | SEQUENCE -> "SEQUENCE"
  | ASSIGN -> "ASSIGN"         | COLUMN -> "COLUMN"
  | DOT -> "DOT"
  (* logical operation *)
  | AND -> "AND"      | OR -> "OR"
  | NOT -> "NOT"      | IF -> "IF"
  | ELSE -> "ELSE"    | FOR -> "FOR"
  | WHILE -> "WHILE"  | BREAK -> "BREAK"  
  | CONTINUE -> "CONTINUE" | IN -> "IN"
  (* comparator *)
  | EQUAL -> "EQUAL"          | NOTEQUAL -> "NOTEQUAL"
  | GREATER -> "GREATER"      | GREATEREQUAL -> "GREATEREQUAL"
  | SMALLER -> "SMALLER"      | SMALLEREQUAL -> "SMALLEREQUAL"
  (* graph operator *)
  | LINK -> "LINK"            | RIGHTLINK -> "RIGHTLINK"
  | LEFTLINK -> "LEFTLINK"    | AT -> "AT"
  | AMPERSAND -> "AMPERSAND"  | SIMILARITY -> "SIMILARITY"
  (* identifier *)
  | ID(string) -> "ID"
  (* primary type *)
  | INT -> "INT"          | FLOAT -> "FLOAT"
  | STRING -> "STRING"    | BOOL -> "BOOL"
  | NODE -> "NODE"        | GRAPH -> "GRAPH"
  | LIST -> "LIST"        | DICT -> "DICT"
  | NULL -> "NULL"
  (* quote *)
  | QUOTE -> "QUOTE"
  (* boolean operation *)
  (* bracket *)
  | LEFTBRACKET -> "LEFTBRACKET"           | RIGHTBRACKET -> "RIGHTBRACKET"
  | LEFTCURLYBRACKET -> "LEFTCURLYBRACKET" | RIGHTCURLYBRACKET -> "RIGHTCURLYBRACKET"
  | LEFTROUNDBRACKET -> "LEFTROUNDBRACKET" | RIGHTROUNDBRACKET -> "RIGHTROUNDBRACKET"
  (* End-of-File *)
  | EOF -> "EOF"
  (* Literals *)
  | INT_LITERAL(int) -> "INT_LITERAL"
  | FLOAT_LITERAL(float) -> "FLOAT_LITERAL"
  | STRING_LITERAL(string) -> "STRING_LITERAL"
  | BOOL_LITERAL(bool) -> "BOOL_LITERAL"
  | RETURN -> "RETURN"

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let rec print_tokens = function
    | EOF -> " "
    | token ->
      print_endline (stringify token);
      print_tokens (Scanner.token lexbuf) in
  print_tokens (Scanner.token lexbuf)

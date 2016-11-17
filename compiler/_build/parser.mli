type token =
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | MOD
  | SEMICOLUMN
  | SEQUENCE
  | ASSIGN
  | COLUMN
  | DOT
  | GREATER
  | GREATEREQUAL
  | SMALLER
  | SMALLEREQUAL
  | EQUAL
  | NOTEQUAL
  | AND
  | OR
  | NOT
  | IF
  | ELSE
  | FOR
  | WHILE
  | BREAK
  | CONTINUE
  | IN
  | RETURN
  | LINK
  | RIGHTLINK
  | LEFTLINK
  | SIMILARITY
  | AT
  | AMPERSAND
  | INT
  | FLOAT
  | STRING
  | BOOL
  | NODE
  | GRAPH
  | LIST
  | DICT
  | NULL
  | QUOTE
  | LEFTBRACKET
  | RIGHTBRACKET
  | LEFTCURLYBRACKET
  | RIGHTCURLYBRACKET
  | LEFTROUNDBRACKET
  | RIGHTROUNDBRACKET
  | EOF
  | ID of (string)
  | INT_LITERAL of (int)
  | STRING_LITERAL of (string)
  | FLOAT_LITERAL of (float)
  | BOOL_LITERAL of (bool)

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf ->  Ast.program

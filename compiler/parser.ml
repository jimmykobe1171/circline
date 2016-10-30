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
  | BREAK
  | CONTINUE
  | IN
  | RETURN
  | LINK
  | RIGHTLINK
  | LEFTLINK
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
  | TRUE
  | FALSE
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

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
 open Ast 
# 60 "parser.ml"
let yytransl_const = [|
  257 (* PLUS *);
  258 (* MINUS *);
  259 (* TIMES *);
  260 (* DIVIDE *);
  261 (* MOD *);
  262 (* SEMICOLUMN *);
  263 (* SEQUENCE *);
  264 (* ASSIGN *);
  265 (* COLUMN *);
  266 (* DOT *);
  267 (* GREATER *);
  268 (* GREATEREQUAL *);
  269 (* SMALLER *);
  270 (* SMALLEREQUAL *);
  271 (* EQUAL *);
  272 (* NOTEQUAL *);
  273 (* AND *);
  274 (* OR *);
  275 (* NOT *);
  276 (* IF *);
  277 (* ELSE *);
  278 (* FOR *);
  279 (* BREAK *);
  280 (* CONTINUE *);
  281 (* IN *);
  282 (* RETURN *);
  283 (* LINK *);
  284 (* RIGHTLINK *);
  285 (* LEFTLINK *);
  286 (* INT *);
  287 (* FLOAT *);
  288 (* STRING *);
  289 (* BOOL *);
  290 (* NODE *);
  291 (* GRAPH *);
  292 (* LIST *);
  293 (* DICT *);
  294 (* NULL *);
  295 (* QUOTE *);
  296 (* TRUE *);
  297 (* FALSE *);
  298 (* LEFTBRACKET *);
  299 (* RIGHTBRACKET *);
  300 (* LEFTCURLYBRACKET *);
  301 (* RIGHTCURLYBRACKET *);
  302 (* LEFTROUNDBRACKET *);
  303 (* RIGHTROUNDBRACKET *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  304 (* ID *);
  305 (* INT_LITERAL *);
  306 (* STRING_LITERAL *);
  307 (* FLOAT_LITERAL *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\002\000\002\000\000\000"

let yylen = "\002\000\
\001\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\002\000\003\000\002\000\
\001\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\017\000\018\000\000\000\001\000\
\000\000\014\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\004\000\005\000\013\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000"

let yydgoto = "\002\000\
\007\000\008\000"

let yysindex = "\006\000\
\254\254\000\000\254\254\254\254\000\000\000\000\076\255\000\000\
\041\255\000\000\254\254\254\254\254\254\254\254\254\254\254\254\
\254\254\254\254\254\254\254\254\254\254\254\254\254\254\041\255\
\041\255\000\000\000\000\000\000\001\255\001\255\001\255\001\255\
\027\255\027\255\009\255\009\255"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\008\000\000\000\
\001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\009\000\
\027\000\000\000\000\000\000\000\019\000\035\000\043\000\051\000\
\055\000\059\000\061\000\063\000"

let yygindex = "\000\000\
\053\000\000\000"

let yytablesize = 337
let yytable = "\003\000\
\016\000\011\000\012\000\013\000\014\000\015\000\001\000\019\000\
\002\000\011\000\012\000\013\000\014\000\015\000\000\000\000\000\
\004\000\000\000\010\000\016\000\017\000\018\000\019\000\020\000\
\021\000\000\000\003\000\011\000\012\000\013\000\014\000\015\000\
\000\000\000\000\011\000\000\000\000\000\016\000\017\000\018\000\
\019\000\000\000\008\000\013\000\014\000\015\000\005\000\000\000\
\006\000\000\000\009\000\000\000\000\000\000\000\006\000\009\000\
\010\000\000\000\007\000\000\000\012\000\000\000\015\000\024\000\
\025\000\026\000\027\000\028\000\029\000\030\000\031\000\032\000\
\033\000\034\000\035\000\036\000\011\000\012\000\013\000\014\000\
\015\000\000\000\000\000\000\000\000\000\000\000\016\000\017\000\
\018\000\019\000\020\000\021\000\022\000\023\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\016\000\016\000\000\000\000\000\000\000\000\000\000\000\
\000\000\002\000\002\000\016\000\016\000\016\000\016\000\016\000\
\016\000\016\000\016\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\003\000\003\000\010\000\010\000\010\000\
\010\000\010\000\010\000\010\000\010\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\011\000\011\000\011\000\
\011\000\011\000\011\000\011\000\011\000\008\000\008\000\008\000\
\008\000\008\000\008\000\008\000\008\000\009\000\009\000\009\000\
\009\000\009\000\009\000\009\000\009\000\006\000\006\000\006\000\
\006\000\007\000\007\000\007\000\007\000\012\000\012\000\015\000\
\015\000"

let yycheck = "\002\001\
\000\000\001\001\002\001\003\001\004\001\005\001\001\000\000\000\
\000\000\001\001\002\001\003\001\004\001\005\001\255\255\255\255\
\019\001\255\255\000\000\011\001\012\001\013\001\014\001\015\001\
\016\001\255\255\000\000\001\001\002\001\003\001\004\001\005\001\
\255\255\255\255\000\000\255\255\255\255\011\001\012\001\013\001\
\014\001\255\255\000\000\003\001\004\001\005\001\049\001\255\255\
\051\001\255\255\000\000\255\255\255\255\255\255\000\000\003\000\
\004\000\255\255\000\000\255\255\000\000\255\255\000\000\011\000\
\012\000\013\000\014\000\015\000\016\000\017\000\018\000\019\000\
\020\000\021\000\022\000\023\000\001\001\002\001\003\001\004\001\
\005\001\255\255\255\255\255\255\255\255\255\255\011\001\012\001\
\013\001\014\001\015\001\016\001\017\001\018\001\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\001\001\002\001\255\255\255\255\255\255\255\255\255\255\
\255\255\001\001\002\001\011\001\012\001\013\001\014\001\015\001\
\016\001\017\001\018\001\011\001\012\001\013\001\014\001\015\001\
\016\001\017\001\018\001\001\001\002\001\011\001\012\001\013\001\
\014\001\015\001\016\001\017\001\018\001\011\001\012\001\013\001\
\014\001\015\001\016\001\017\001\018\001\011\001\012\001\013\001\
\014\001\015\001\016\001\017\001\018\001\011\001\012\001\013\001\
\014\001\015\001\016\001\017\001\018\001\011\001\012\001\013\001\
\014\001\015\001\016\001\017\001\018\001\015\001\016\001\017\001\
\018\001\015\001\016\001\017\001\018\001\017\001\018\001\017\001\
\018\001"

let yynames_const = "\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIVIDE\000\
  MOD\000\
  SEMICOLUMN\000\
  SEQUENCE\000\
  ASSIGN\000\
  COLUMN\000\
  DOT\000\
  GREATER\000\
  GREATEREQUAL\000\
  SMALLER\000\
  SMALLEREQUAL\000\
  EQUAL\000\
  NOTEQUAL\000\
  AND\000\
  OR\000\
  NOT\000\
  IF\000\
  ELSE\000\
  FOR\000\
  BREAK\000\
  CONTINUE\000\
  IN\000\
  RETURN\000\
  LINK\000\
  RIGHTLINK\000\
  LEFTLINK\000\
  INT\000\
  FLOAT\000\
  STRING\000\
  BOOL\000\
  NODE\000\
  GRAPH\000\
  LIST\000\
  DICT\000\
  NULL\000\
  QUOTE\000\
  TRUE\000\
  FALSE\000\
  LEFTBRACKET\000\
  RIGHTBRACKET\000\
  LEFTCURLYBRACKET\000\
  RIGHTCURLYBRACKET\000\
  LEFTROUNDBRACKET\000\
  RIGHTROUNDBRACKET\000\
  EOF\000\
  "

let yynames_block = "\
  ID\000\
  INT_LITERAL\000\
  STRING_LITERAL\000\
  FLOAT_LITERAL\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'literals) in
    Obj.repr(
# 59 "parser.mly"
           (_1)
# 312 "parser.ml"
               :  Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 :  Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 :  Ast.expr) in
    Obj.repr(
# 60 "parser.mly"
                              ( Binop(_1, Add,   _3) )
# 320 "parser.ml"
               :  Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 :  Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 :  Ast.expr) in
    Obj.repr(
# 61 "parser.mly"
                              ( Binop(_1, Sub,   _3) )
# 328 "parser.ml"
               :  Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 :  Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 :  Ast.expr) in
    Obj.repr(
# 62 "parser.mly"
                              ( Binop(_1, Mult,  _3) )
# 336 "parser.ml"
               :  Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 :  Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 :  Ast.expr) in
    Obj.repr(
# 63 "parser.mly"
                              ( Binop(_1, Div,   _3) )
# 344 "parser.ml"
               :  Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 :  Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 :  Ast.expr) in
    Obj.repr(
# 64 "parser.mly"
                              ( Binop(_1, Equal, _3) )
# 352 "parser.ml"
               :  Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 :  Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 :  Ast.expr) in
    Obj.repr(
# 65 "parser.mly"
                              ( Binop(_1, Neq,   _3) )
# 360 "parser.ml"
               :  Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 :  Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 :  Ast.expr) in
    Obj.repr(
# 66 "parser.mly"
                              ( Binop(_1, Less,  _3) )
# 368 "parser.ml"
               :  Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 :  Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 :  Ast.expr) in
    Obj.repr(
# 67 "parser.mly"
                              ( Binop(_1, Leq,   _3) )
# 376 "parser.ml"
               :  Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 :  Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 :  Ast.expr) in
    Obj.repr(
# 68 "parser.mly"
                              ( Binop(_1, Greater,  _3) )
# 384 "parser.ml"
               :  Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 :  Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 :  Ast.expr) in
    Obj.repr(
# 69 "parser.mly"
                              ( Binop(_1, Geq,   _3) )
# 392 "parser.ml"
               :  Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 :  Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 :  Ast.expr) in
    Obj.repr(
# 70 "parser.mly"
                              ( Binop(_1, And,   _3) )
# 400 "parser.ml"
               :  Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 :  Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 :  Ast.expr) in
    Obj.repr(
# 71 "parser.mly"
                              ( Binop(_1, Mod,   _3))
# 408 "parser.ml"
               :  Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 :  Ast.expr) in
    Obj.repr(
# 72 "parser.mly"
                           ( Unop (Not,   _2) )
# 415 "parser.ml"
               :  Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 :  Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 :  Ast.expr) in
    Obj.repr(
# 73 "parser.mly"
                              ( Binop(_1, Or,    _3) )
# 423 "parser.ml"
               :  Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 :  Ast.expr) in
    Obj.repr(
# 75 "parser.mly"
                            ( Unop (Sub, _2) )
# 430 "parser.ml"
               :  Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 78 "parser.mly"
              (Num_Lit( Num_Int(_1) ))
# 437 "parser.ml"
               : 'literals))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 79 "parser.mly"
                (Num_Lit( Num_Float(_1) ))
# 444 "parser.ml"
               : 'literals))
(* Entry expr *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let expr (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf :  Ast.expr)

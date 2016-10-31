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
\001\000\002\000\002\000\003\000\004\000\004\000\004\000\004\000\
\004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
\004\000\004\000\004\000\004\000\004\000\004\000\005\000\005\000\
\000\000"

let yylen = "\002\000\
\002\000\000\000\002\000\002\000\001\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\002\000\003\000\001\000\003\000\002\000\001\000\001\000\
\002\000"

let yydefred = "\000\000\
\002\000\000\000\025\000\000\000\000\000\000\000\001\000\000\000\
\023\000\024\000\003\000\000\000\005\000\000\000\018\000\000\000\
\000\000\000\000\000\000\000\000\000\000\004\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\008\000\009\000\017\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000"

let yydgoto = "\002\000\
\003\000\004\000\011\000\012\000\013\000"

let yysindex = "\015\000\
\000\000\000\000\000\000\001\000\254\254\254\254\000\000\029\255\
\000\000\000\000\000\000\039\255\000\000\006\255\000\000\254\254\
\254\254\254\254\254\254\254\254\254\254\000\000\254\254\254\254\
\254\254\254\254\254\254\254\254\254\254\254\254\075\255\006\255\
\006\255\000\000\000\000\000\000\215\255\215\255\215\255\215\255\
\001\255\001\255\147\255\147\255"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\057\255\
\000\000\000\000\000\000\000\000\000\000\093\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\032\255\111\255\
\129\255\000\000\000\000\000\000\158\255\171\255\184\255\197\255\
\018\255\150\255\079\255\097\255"

let yygindex = "\000\000\
\000\000\000\000\000\000\002\000\000\000"

let yytablesize = 308
let yytable = "\005\000\
\007\000\017\000\018\000\019\000\020\000\021\000\014\000\015\000\
\019\000\020\000\021\000\023\000\024\000\025\000\026\000\001\000\
\006\000\031\000\032\000\033\000\034\000\035\000\036\000\010\000\
\037\000\038\000\039\000\040\000\041\000\042\000\043\000\044\000\
\010\000\010\000\010\000\010\000\016\000\021\000\000\000\017\000\
\018\000\019\000\020\000\021\000\022\000\008\000\009\000\000\000\
\010\000\023\000\024\000\025\000\026\000\027\000\028\000\029\000\
\030\000\020\000\020\000\020\000\020\000\020\000\020\000\000\000\
\000\000\000\000\000\000\020\000\020\000\020\000\020\000\020\000\
\020\000\020\000\020\000\017\000\018\000\019\000\020\000\021\000\
\000\000\000\000\000\000\000\000\016\000\023\000\024\000\025\000\
\026\000\027\000\028\000\029\000\030\000\022\000\022\000\016\000\
\016\000\000\000\022\000\000\000\000\000\000\000\019\000\022\000\
\022\000\022\000\022\000\022\000\022\000\022\000\022\000\006\000\
\006\000\019\000\019\000\000\000\006\000\000\000\000\000\000\000\
\000\000\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
\006\000\007\000\007\000\000\000\000\000\000\000\007\000\000\000\
\000\000\000\000\000\000\007\000\007\000\007\000\007\000\007\000\
\007\000\007\000\007\000\017\000\018\000\019\000\020\000\021\000\
\000\000\000\000\000\000\011\000\000\000\023\000\024\000\025\000\
\026\000\027\000\028\000\014\000\011\000\011\000\011\000\011\000\
\014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
\015\000\000\000\000\000\000\000\000\000\015\000\015\000\015\000\
\015\000\015\000\015\000\015\000\015\000\012\000\000\000\000\000\
\000\000\000\000\012\000\012\000\012\000\012\000\012\000\012\000\
\012\000\012\000\013\000\000\000\000\000\000\000\000\000\013\000\
\013\000\013\000\013\000\013\000\013\000\013\000\013\000\017\000\
\018\000\019\000\020\000\021\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\005\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\006\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\008\000\009\000\000\000\010\000"

let yycheck = "\002\001\
\000\000\001\001\002\001\003\001\004\001\005\001\005\000\006\000\
\003\001\004\001\005\001\011\001\012\001\013\001\014\001\001\000\
\019\001\016\000\017\000\018\000\019\000\020\000\021\000\006\001\
\023\000\024\000\025\000\026\000\027\000\028\000\029\000\030\000\
\015\001\016\001\017\001\018\001\008\001\006\001\255\255\001\001\
\002\001\003\001\004\001\005\001\006\001\048\001\049\001\255\255\
\051\001\011\001\012\001\013\001\014\001\015\001\016\001\017\001\
\018\001\001\001\002\001\003\001\004\001\005\001\006\001\255\255\
\255\255\255\255\255\255\011\001\012\001\013\001\014\001\015\001\
\016\001\017\001\018\001\001\001\002\001\003\001\004\001\005\001\
\255\255\255\255\255\255\255\255\006\001\011\001\012\001\013\001\
\014\001\015\001\016\001\017\001\018\001\001\001\002\001\017\001\
\018\001\255\255\006\001\255\255\255\255\255\255\006\001\011\001\
\012\001\013\001\014\001\015\001\016\001\017\001\018\001\001\001\
\002\001\017\001\018\001\255\255\006\001\255\255\255\255\255\255\
\255\255\011\001\012\001\013\001\014\001\015\001\016\001\017\001\
\018\001\001\001\002\001\255\255\255\255\255\255\006\001\255\255\
\255\255\255\255\255\255\011\001\012\001\013\001\014\001\015\001\
\016\001\017\001\018\001\001\001\002\001\003\001\004\001\005\001\
\255\255\255\255\255\255\006\001\255\255\011\001\012\001\013\001\
\014\001\015\001\016\001\006\001\015\001\016\001\017\001\018\001\
\011\001\012\001\013\001\014\001\015\001\016\001\017\001\018\001\
\006\001\255\255\255\255\255\255\255\255\011\001\012\001\013\001\
\014\001\015\001\016\001\017\001\018\001\006\001\255\255\255\255\
\255\255\255\255\011\001\012\001\013\001\014\001\015\001\016\001\
\017\001\018\001\006\001\255\255\255\255\255\255\255\255\011\001\
\012\001\013\001\014\001\015\001\016\001\017\001\018\001\001\001\
\002\001\003\001\004\001\005\001\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\002\001\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\019\001\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\048\001\049\001\255\255\051\001"

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
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 60 "parser.mly"
                                          ( List.rev _1 )
# 309 "parser.ml"
               :  Ast.program))
; (fun __caml_parser_env ->
    Obj.repr(
# 63 "parser.mly"
                                          ( [] )
# 315 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 64 "parser.mly"
                                          ( _2 :: _1 )
# 323 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 67 "parser.mly"
                                          ( Expr(_1) )
# 330 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'literals) in
    Obj.repr(
# 70 "parser.mly"
           (_1)
# 337 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 71 "parser.mly"
                              ( Binop(_1, Add,   _3) )
# 345 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 72 "parser.mly"
                              ( Binop(_1, Sub,   _3) )
# 353 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 73 "parser.mly"
                              ( Binop(_1, Mult,  _3) )
# 361 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 74 "parser.mly"
                              ( Binop(_1, Div,   _3) )
# 369 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 75 "parser.mly"
                              ( Binop(_1, Equal, _3) )
# 377 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 76 "parser.mly"
                              ( Binop(_1, Neq,   _3) )
# 385 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 77 "parser.mly"
                              ( Binop(_1, Less,  _3) )
# 393 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 78 "parser.mly"
                              ( Binop(_1, Leq,   _3) )
# 401 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 79 "parser.mly"
                              ( Binop(_1, Greater,  _3) )
# 409 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 80 "parser.mly"
                              ( Binop(_1, Geq,   _3) )
# 417 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 81 "parser.mly"
                              ( Binop(_1, And,   _3) )
# 425 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 82 "parser.mly"
                              ( Binop(_1, Mod,   _3))
# 433 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 83 "parser.mly"
                       ( Unop (Not,   _2) )
# 440 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 84 "parser.mly"
                                            ( Binop(_1, Or,    _3) )
# 448 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 85 "parser.mly"
                              ( Id(_1) )
# 455 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 86 "parser.mly"
                              ( Assign(_1, _3) )
# 463 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 87 "parser.mly"
                        ( Unop (Sub, _2) )
# 470 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 90 "parser.mly"
              (Num_Lit( Num_Int(_1) ))
# 477 "parser.ml"
               : 'literals))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 91 "parser.mly"
                (Num_Lit( Num_Float(_1) ))
# 484 "parser.ml"
               : 'literals))
(* Entry program *)
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
let program (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf :  Ast.program)

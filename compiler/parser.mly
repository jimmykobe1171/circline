
%{ open Ast %}

/* Arithmetic Operators */
%token PLUS MINUS TIMES DIVIDE MOD

/* Separator */
%token SEMICOLUMN SEQUENCE ASSIGN COLUMN DOT

/* Relational Operators */
%token GREATER GREATEREQUAL SMALLER SMALLEREQUAL EQUAL NOTEQUAL

/* Logical Operators & Keywords*/
%token AND OR NOT IF ELSE FOR BREAK CONTINUE IN RETURN

/* Graph operator */
%token LINK RIGHTLINK LEFTLINK

/* Primary Type */
%token INT FLOAT STRING BOOL NODE GRAPH LIST DICT NULL

/* Quote */
%token QUOTE

/* Boolean  */
%token TRUE FALSE

/* Bracket */
%token LEFTBRACKET RIGHTBRACKET LEFTCURLYBRACKET RIGHTCURLYBRACKET LEFTROUNDBRACKET RIGHTROUNDBRACKET
/* EOF */
%token EOF

/* Identifiers */
%token <string> ID

/* Literals */
%token <int> INT_LITERAL
%token <string> STRING_LITERAL
%token <float> FLOAT_LITERAL

/* Order */
%right ASSIGN 
%left LINK RIGHTLINK LEFTLINK
%left AND OR
%left EQUAL NOTEQUAL
%left GREATER SMALLER GREATEREQUAL SMALLEREQUAL
%left PLUS MINUS
%left TIMES DIVIDE MOD
%right NOT
%right LEFTROUNDBRACKET
%left  RIGHTROUNDBRACKET
%right COLUMN
%right DOT

%start program
%type < Ast.program> program

%%

/* Program flow */
program:
| stmt_list EOF                         { List.rev $1 }

stmt_list:
| /* nothing */                         { [] }
| stmt_list stmt                        { $2 :: $1 }

stmt:
| expr SEMICOLUMN                       { Expr($1) }
| func_decl                             { Func($1) }
| RETURN expr SEMICOLUMN                { Return($2) }


var_type:
  INT 								  	{Int_t}
| FLOAT 								{Float_t}
| STRING 								{String_t}

formal_list:
| /* nothing */               { [] }
| formal                      { [$1] }
| formal_list SEQUENCE formal { $3 :: $1 }


formal:
| var_type ID           { Formal($1, $2) }

func_decl:
| var_type ID LEFTROUNDBRACKET formal_list RIGHTROUNDBRACKET LEFTCURLYBRACKET stmt_list RIGHTCURLYBRACKET {
  {
    returnType = $1;
    name = $2;
    args = List.rev $4;
    body = List.rev $7;
  }
}

expr:
  literals {$1}
| arith_ops                       { $1 }
| graph_ops                       { $1 }
| NODE LEFTROUNDBRACKET expr RIGHTROUNDBRACKET { Node($3) }
| ID 					                    { Id($1) }
| ID ASSIGN expr 					        { Assign($1, $3) }
| LEFTBRACKET list RIGHTBRACKET   			{ ListP(List.rev $2) }
| LEFTCURLYBRACKET dict RIGHTCURLYBRACKET 	{ DictP(List.rev $2) }
| LEFTROUNDBRACKET expr RIGHTROUNDBRACKET 	{ $2 }
| STRING_LITERAL COLUMN expr 				{Dict_Key_Value(String_lit($1), $3)}
| ID LEFTROUNDBRACKET list RIGHTROUNDBRACKET              { Call($1, $3) }


/* Lists */
list:
| /* nothing */                         { [] }
| expr                                  { [$1] }
| list SEQUENCE expr                    { $3 :: $1 }

/* dict */
dict:
| /* nothing */                     { [] }
| expr 								{ [$1] }
| dict SEQUENCE expr 				{$3 :: $1}


arith_ops:
| expr PLUS         expr 					{ Binop($1, Add,   $3) }
| expr MINUS        expr 					{ Binop($1, Sub,   $3) }
| expr TIMES        expr 					{ Binop($1, Mult,  $3) }
| expr DIVIDE       expr 					{ Binop($1, Div,   $3) }
| expr EQUAL        expr 					{ Binop($1, Equal, $3) }
| expr NOTEQUAL     expr 					{ Binop($1, Neq,   $3) }
| expr SMALLER      expr 					{ Binop($1, Less,  $3) }
| expr SMALLEREQUAL expr 					{ Binop($1, Leq,   $3) }
| expr GREATER      expr 					{ Binop($1, Greater,  $3) }
| expr GREATEREQUAL expr 					{ Binop($1, Geq,   $3) }
| expr AND          expr 					{ Binop($1, And,   $3) }
| expr MOD          expr 					{ Binop($1, Mod,   $3) }
| expr OR     expr                { Binop($1, Or,    $3) }
| NOT  expr 							        { Unop (Not,   $2) }
| MINUS expr 							        { Unop (Sub, $2) }

graph_ops:
| expr LINK expr                  { Graph_Binop($1, Double_Link, $3) }
| expr RIGHTLINK expr             { Graph_Binop($1, Right_Link, $3) }
| expr LEFTLINK expr              { Graph_Binop($1, Left_Link, $3) }

literals:
  INT_LITERAL   	{Num_Lit( Num_Int($1) )}
| FLOAT_LITERAL 	{Num_Lit( Num_Float($1) )}
| STRING_LITERAL    { String_lit($1) }

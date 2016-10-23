
%{ open Ast %}

/* Arithmetic Operators */
%token PLUS MINUS TIMES DIVIDE MOD

/* Separator */
%token SEMICOLUMN SEQUENCE ASSIGN COLUMN DOT

/* Relational Operators */
%token GREATER GREATEREQUAL SMALLER SMALLEREQUAL

/* Logical Operators & Keywords*/
%token AND OR NOT IF ELSE FOR BREAK CONTINUE IN

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



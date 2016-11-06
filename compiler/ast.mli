(* Binary Operators *)
type binop =
  Add         (* + *)
| Sub         (* - *)
| Mult        (* * *)
| Div         (* / *)
| Mod         (* % *)
| Equal       (* == *)
| Neq         (* != *)
| Less        (* < *)
| Leq         (* <= *)
| Greater     (* > *)
| Geq         (* >= *)
| And         (* and *)
| Or          (* or *)

(* Unary Operators *)
type unop =
  Sub         (* - *)
| Not         (* not *)

(* Numbers int | float *)
type num =
  Num_Int of int          (* 514 *)
| Num_Float of float      (* 3.1415 *)

(* Variable Type *)
type var_type =
  Int_t                   (* int *)
| Float_t                 (* float *)
| String_t                (* string *)

(* Type Declaration *)
type formal =
| Formal of var_type * string   (* int aNum *)

type graph_binop =
| Right_Link
| Left_Link
| Double_Link

type expr =
    Num_Lit of num
|   String_lit of string      
|   Node of expr
| 	Graph_Binop of expr * graph_binop * expr
| 	Binop of expr * binop * expr
|  	Unop of unop * expr
|   Id of string
|   Assign of string * expr
|   ListP of expr list
|   DictP of expr list
|   Dict_Key_Value of expr * expr 

(* Statements *)
type stmt =
  Expr of expr     (* set foo = bar + 3 *)
| Func of func_decl

(* Function Declaration *)
and func_decl = {
  returnType: var_type;
  body: stmt list;
  args: formal list;
  name: string;
}

(* Program entry point *)
type program = stmt list

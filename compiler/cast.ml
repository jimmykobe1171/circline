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
(* Graph Only *)
| ListNodesAt        (* <graph> @  <node> *)
| ListEdgesAt        (* <graph> @@ <node> *)
| RootAs             (* <graph> ~  <node> *)

(* Unary Operators *)
type unop =
  Neg         (* - *)
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
| Bool_t
| Node_t
| Graph_t
| Dict_Int_t
| Dict_Float_t
| Dict_String_t
| Dict_Node_t
| Dict_Graph_t
| List_Int_t
| List_Float_t
| List_String_t
| List_Node_t
| List_Graph_t
| Void_t
| Null_t

(* Type Declaration *)
type formal =
| Formal of var_type * string   (* int aNum *)

type graph_op =
| Right_Link
| Left_Link
| Double_Link

type expr =
    Num_Lit of num
|   Null
|   String_Lit of string
|   Bool_lit of bool
|   Node of int * expr
|   Graph_Link of expr * graph_op * expr * expr
|   Binop of expr * binop * expr
|   Unop of unop * expr
|   Id of string
|   Assign of string * expr
|   Noexpr
|   ListP of expr list
|   DictP of (expr * expr) list
|   Call of string * expr list    (* function call *)
|   CallDefault of expr * string * expr list

and edge_graph_list = {
  graphs: expr list;
  edges: expr list;
}

type var_decl =
| Local of var_type * string * expr

(* Statements *)
type stmt =
  Expr of expr     (* set foo = bar + 3 *)
| Return of expr
| For of expr * expr * expr * stmt list
| If of expr * stmt list * stmt list
| While of expr * stmt list

(* Function Declaration *)
and func_decl = {
  returnType: var_type;
  name: string;
  args: formal list;
  body: stmt list;
  locals: formal list;
  pname: string; (* parent func name *)
}


(* Program entry point *)
type program = func_decl list

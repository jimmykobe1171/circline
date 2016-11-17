open Ast
open Printf

(* Unary operators *)
let txt_of_unop = function
  | Not -> "Not"
  | Sub -> "Sub"

(* Binary operators *)
let txt_of_binop = function
  (* Arithmetic *)
  | Add -> "Add"
  | Sub -> "Sub"
  | Mult -> "Mult"
  | Div -> "Div"
  | Mod -> "Mod"
  (* Boolean *)
  | Or -> "Or"
  | And -> "And"
  | Equal -> "Equal"
  | Neq -> "Neq"
  | Less -> "Less"
  | Leq -> "Leq"
  | Greater -> "Greater"
  | Geq -> "Geq"
  (* Graph *)
  | ListNodesAt -> "Child_Nodes_At"
  | ListEdgesAt -> "Child_Nodes&Edges_At"
  | RootAs -> "Root_As"

let txt_of_graph_op = function
  | Right_Link -> "RLink"
  | Left_Link -> "LLink"
  | Double_Link -> "DLink"

let txt_of_var_type = function
  | Void_t -> "void"
  | Null_t -> "null"
  | Int_t -> "int"
  | Float_t -> "float"
  | String_t -> "string"
  | Bool_t -> "bool"
  | Node_t -> "node"
  | Graph_t -> "graph"
  | List_t -> "list"
  | Dict_Int_t -> "dict<int>"
  | Dict_Float_t -> "dict<float>"
  | Dict_String_t -> "dict<string>"
  | Dict_Node_t -> "dict<node>"
  | Dict_Graph_t -> "dict<graph>"

let txt_of_formal = function
| Formal(vtype, name) -> sprintf "Formal(%s, %s)" (txt_of_var_type vtype) name


let txt_of_formal_list formals =
  let rec aux acc = function
    | [] -> sprintf "%s" (String.concat ", " (List.rev acc))
    | fml :: tl -> aux (txt_of_formal fml :: acc) tl
  in aux [] formals

let txt_of_num = function
  | Num_Int(x) -> string_of_int x
  | Num_Float(x) -> string_of_float x

(* Expressions *)
let rec txt_of_expr = function
  | Num_Lit(x) -> sprintf "Num_Lit(%s)" (txt_of_num x)
  | Bool_lit(x) -> sprintf "Bool_lit(%s)" (string_of_bool x)
  | String_Lit(x) -> sprintf "String_Lit(%s)" x
  | Null -> sprintf "Null"
  | Node(x) -> sprintf "Node(%s)" (txt_of_expr x)
  | Unop(op, e) -> sprintf "Unop(%s, %s)" (txt_of_unop op) (txt_of_expr e)
  | Binop(e1, op, e2) -> sprintf "Binop(%s, %s, %s)"
      (txt_of_expr e1) (txt_of_binop op) (txt_of_expr e2)
  | Graph_Link(e1, op1, e2, e3) -> sprintf "Graph_Link(%s, %s, %s, WithEdge, %s)"
      (txt_of_expr e1) (txt_of_graph_op op1) (txt_of_expr e2) (txt_of_expr e3)
  | Id(x) -> sprintf "Id(%s)" x
  | Assign(e1, e2) -> sprintf "Assign(%s, %s)" e1 (txt_of_expr e2)
  | Noexpr -> sprintf "Noexpression"
  | ListP(l) -> sprintf "List(%s)" (txt_of_list l)
  | DictP(d) -> sprintf "Dict(%s)" (txt_of_dict d)
  | Call(f, args) -> sprintf "Call(%s, [%s])" (f) (txt_of_list args)

(*Variable Declaration*)
and txt_of_var_decl = function
  | Local(var, name, e1) -> sprintf "Local(%s, %s, %s)"
    (txt_of_var_type var) name (txt_of_expr e1)

(* Lists *)
and txt_of_list = function
  | [] -> ""
  | [x] -> txt_of_expr x
  | _ as l -> String.concat ", " (List.map txt_of_expr l)

(* Dict *)
and txt_of_dict_key_value = function
  | (key, value) -> sprintf "key:%s,value:%s" (txt_of_expr key) (txt_of_expr value)

and txt_of_dict = function
  | [] -> ""
  | [x] -> txt_of_dict_key_value x
  | _ as d -> String.concat ", " (List.map txt_of_dict_key_value d)

(* Functions Declaration *)
and txt_of_func_decl f =
  sprintf "%s %s (%s) {%s}"
    (txt_of_var_type f.returnType) f.name (txt_of_formal_list f.args) (txt_of_stmts f.body)

(* Statements *)
and txt_of_stmt = function
  | Expr(expr) -> sprintf "Expr(%s);" (txt_of_expr expr)
  | Func(f) -> sprintf "Func(%s)" (txt_of_func_decl f)
  | Return(expr) -> sprintf "Return(%s);" (txt_of_expr expr)
  | For(e1,e2,e3,s) -> sprintf "For(%s;%s;%s){%s}"
    (txt_of_var_decl e1) (txt_of_expr e2) (txt_of_expr e3) (txt_of_stmts s)
  | If(e1,s1,s2) -> sprintf "If(%s){%s} Else{%s}"
    (txt_of_expr e1) (txt_of_stmts s1) (txt_of_stmts s2)
  | While(e1, s) -> sprintf "While(%s){%s}"
    (txt_of_expr e1) (txt_of_stmts s)
  | Var_dec(var) -> sprintf "Var_dec(%s);" (txt_of_var_decl var)
and txt_of_stmts stmts =
  let rec aux acc = function
      | [] -> sprintf "%s" (String.concat "\n" (List.rev acc))
      | stmt :: tl -> aux (txt_of_stmt stmt :: acc) tl
  in aux [] stmts

(* Program entry point *)
let _ =
  let lexbuf = Lexing.from_channel stdin in
  let program = Parser.program Scanner.token lexbuf in
  let result = txt_of_stmts program in
  print_endline result

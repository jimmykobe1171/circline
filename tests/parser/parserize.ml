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

let txt_of_var_type = function
  | Int_t -> "int"
  | Float_t -> "float"
  | String_t -> "string"

let txt_of_formal = function
| Formal(vtype, name) -> sprintf "%s %s" (txt_of_var_type vtype) name

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
  | Unop(op, e) -> sprintf "Unop(%s, %s)" (txt_of_unop op) (txt_of_expr e)
  | Binop(e1, op, e2) -> sprintf "Binop(%s, %s, %s)"
      (txt_of_expr e1) (txt_of_binop op) (txt_of_expr e2)
  | Id(x) -> sprintf "Id(%s)" x
  | Assign(e1, e2) -> sprintf "Assign(%s, %s)" e1 (txt_of_expr e2)
  | ListP(l) -> sprintf "List(%s)" (txt_of_list l)

(* Lists *)
and txt_of_list = function
  | [] -> ""
  | [x] -> txt_of_expr x
  | _ as l -> String.concat ", " (List.map txt_of_expr l)

(* Functions Declaration *)
and txt_of_func_decl f =
  sprintf "%s %s (%s) {%s}"
    (txt_of_var_type f.returnType) f.name (txt_of_formal_list f.args) (txt_of_stmts f.body)

(* Statements *)
and txt_of_stmt = function
  | Expr(expr) -> sprintf "Expr(%s);" (txt_of_expr expr)
  | Func(f) -> sprintf "Func(%s)" (txt_of_func_decl f)

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

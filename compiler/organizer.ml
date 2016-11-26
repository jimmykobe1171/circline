module A = Ast
module C = Cast

let convert_binop = function
    A.Add -> C.Add
  | A.Sub -> C.Sub
  | A.Mult -> C.Mult
  | A.Div -> C.Div
  | A.Mod -> C.Mod 
  | A.Equal -> C.Equal
  | A.Neq -> C.Neq
  | A.Less -> C.Less
  | A.Leq -> C.Leq 
  | A.Greater -> C.Leq
  | A.Geq -> C.Geq
  | A.And -> C.And
  | A.Or -> C.Or
  | A.ListNodesAt -> C.ListNodesAt 
  | A.ListEdgesAt -> C.ListEdgesAt
  | A.RootAs -> C.RootAs

let convert_unop = function
  A.Neg -> C.Neg 
| A.Not -> C.Not

let convert_num = function
    A.Num_Int(a) -> C.Num_Int(a)
  | A.Num_Float(a) -> C.Num_Float(a)

let convert_var_type = function
    A.Int_t -> C.Int_t                
  | A.Float_t -> C.Float_t                
  | A.String_t -> C.String_t
  | A.Bool_t -> C.Bool_t
  | A.Node_t -> C.Node_t
  | A.Graph_t -> C.Graph_t
  | A.List_Int_t
  | A.List_Float_t
  | A.List_String_t
  | A.List_Node_t
  | A.List_Graph_t
  | A.Dict_Int_t -> C.Dict_Int_t
  | A.Dict_Float_t -> C.Dict_Float_t
  | A.Dict_String_t -> C.Dict_String_t
  | A.Dict_Node_t -> C.Dict_Node_t
  | A.Dict_Graph_t -> C.Dict_Graph_t
  | A.Void_t -> C.Void_t
  | A.Null_t -> C.Null_t

let convert_graph_op = function
| A.Right_Link -> C.Right_Link
| A.Left_Link -> C.Right_Link
| A.Double_Link -> C.Double_Link

let rec convert_expr = function
    A.Num_Lit(a) -> C.Num_Lit(convert_num a)
|   A.Null -> C.Null
|   A.String_Lit(a) -> C.String_Lit(a)
|   A.Bool_lit(a) -> C.Bool_lit(a)
|   A.Node(a) -> C.Node(convert_expr a)
|   A.Graph_Link(a,b,c,d) -> C.Graph_Link(convert_expr a, convert_graph_op b, convert_expr c, convert_expr d)
|   A.Binop(a,b,c) -> C.Binop(convert_expr a, convert_binop b, convert_expr c)
|   A.Unop(a,b) -> C.Unop(convert_unop a, convert_expr b)
|   A.Id(a) -> C.Id(a)
|   A.Assign(a,b) -> C.Assign(a, convert_expr b)
|   A.Noexpr -> C.Noexpr
|   A.ListP(a) -> C.ListP(convert_expr_list a)
|   A.DictP(a) -> C.DictP(convert_dict_list a)
|   A.Call(a,b) -> C.Call(a, convert_expr_list b) 
|   A.CallDefault(a,b,c) -> C.CallDefault(convert_expr a, b, convert_expr_list c)

and convert_expr_list = function
    [] -> []
  | [x] -> [convert_expr x]
  | _ as l -> (List.map convert_expr l)

and convert_dict = function
  (c,d) -> (convert_expr c, convert_expr d)

and convert_dict_list = function
    [] -> []
  | [x] -> [convert_dict x]
  | _ as l -> (List.map convert_dict l)

(*)
let convert_stmt = function
    [] -> []
  | A.Expr(a) -> C.Expr(convert_expr a)

let rec convert curr_list = function
   [] -> curr_list
  | s::ss -> (convert_stmt s) :: (convert curr_list ss)
*)
let convert_formal = function
  | A.Formal(v, s) -> C.Formal(convert_var_type v, s) 

let convert_formal_list = function
    [] -> []
  | [x] -> [convert_formal x]
  | _ as l -> (List.map convert_formal l)


let createMain stmts = A.Func({
    returnType = Int_t;
    name = "main";
    args = [];
    body = stmts;
  })


(*
let convertAST stmts =
  let (functions, variables, scripts) =
    let aux (fs, vars, others)= function
      | A.Func(_) as f -> (f::fs, vars, others)
      | A.Var_dec(A.Local(typ, name, v)) -> (
          match v with
          | A.Noexpr -> (fs, A.Var_dec(A.Local(typ, name, A.Noexpr))::vars, others)
          | _ -> (fs, A.Var_dec(A.Local(typ, name, A.Noexpr))::vars, A.Expr(A.Assign(name, v))::others)
        )
      | _ as other -> (fs, vars, other::others)
    in List.fold_left aux ([], [], []) stmts
  in (List.rev functions), (List.rev variables), (List.rev scripts) *)

(* let get_local_from_body = function *)

module StringMap = Map.Make(String)

let rec get_funcs_from_body_a = function
    [] -> []
  | A.Func(_) as x::tl -> x :: (get_funcs_from_body_a tl)
  | _::tl -> get_funcs_from_body_a tl

let rec get_body_from_body_a = function
    [] -> []
  | A.Func(_)::tl -> get_funcs_from_body_a tl
  | _ as x::tl -> x :: (get_funcs_from_body_a tl)

let rec convert_func_list m parent = function
    [] -> ([], m)
  | A.Func{returnType = r; name = n; args = a; body = b}::tl -> 
    let m = StringMap.add n parent m in 
    print_int (StringMap.cardinal m); 
    print_string " is the size!\n";
    (A.Func({
      A.returnType = r; A.name = n; A.args = a; A.body = get_body_from_body_a b
    }) :: (fst (convert_func_list m parent tl)) @ (fst (convert_func_list m n (get_funcs_from_body_a b)))),m
  | _::tl -> (fst (convert_func_list m parent tl),m)


let rec convert_stmt = function
    A.Expr(a) -> C.Expr(convert_expr a)
  | A.Return(a) -> C.Return(convert_expr a)
  | A.For(e1, e2, e3, stls) -> C.For(convert_expr e1, convert_expr e2, convert_expr e3, List.map convert_stmt stls)
  | A.If(e, stls1, stls2) -> C.If(convert_expr e, List.map convert_stmt stls1, List.map convert_stmt stls2)
  | A.While(e, stls) -> C.While(convert_expr e, List.map convert_stmt stls)
  | A.Block(stls) ->C.Block(List.map convert_stmt stls)

(*
let convert_stmt_list = function
    [] -> []
  | [x] -> [convert_stmt x]
  | _ as l -> (List.map convert_stmt l) *)

let rec get_body_from_body_c = function 
    [] -> []
  | A.Var_dec(A.Local(typ, name, v))::tl -> C.Expr(C.Assign(name, convert_expr v)) :: (get_body_from_body_c tl)
  | _ as x::tl -> (convert_stmt x) :: (get_body_from_body_c tl)


let rec get_local_from_body_c = function 
    [] -> []
  | A.Var_dec(A.Local(typ, name, v))::tl -> C.Formal(convert_var_type typ, name) :: (get_local_from_body_c tl)
  | _::tl -> get_local_from_body_c tl


let rec convert_func_list_c m = function
    [] -> []
  | A.Func{returnType = r; name = n; args = a; body = b} :: tl -> {
    C.returnType = convert_var_type r;
    C.name = n;
    C.args = convert_formal_list a;
    C.body = get_body_from_body_c b;
    C.locals = get_local_from_body_c b;
    C.pname = (if n = "main" then "main" else StringMap.find n m)
  } :: (convert_func_list_c m tl)


let rec changes = function 
    [] -> []
  | _ as x :: tl -> A.Func(x) :: (changes tl)


let convert stmts = 
  let funcs = createMain stmts in
  let horizen_funcs_m = convert_func_list StringMap.empty "main" [funcs] in
  print_int (StringMap.cardinal (snd horizen_funcs_m)); convert_func_list_c (snd horizen_funcs_m) (fst horizen_funcs_m)




(*
(* Variable Type *)




let Convert_graph_op = function
  | A.Right_Link -> C.Right_Link
  | A.Left_Link -> C.Left_Link
  | A.Double_Link -> C.Double_Link

let Convert_expr =
    A.Num_Lit(n) -> C.Num_Lit(n)
|   A.Null -> C.Null
|   A.String_Lit -> C.String_Lit
|   A.Bool_lit -> B.Bool_Lit
|   A.Node -> B.Node
|   A.Graph_Link(e1, op, e2, e3) -> C.Graph
|   Graph_Link of expr * graph_op * expr * expr
|   Binop of expr * binop * expr
|   Unop of unop * expr
|   Id of string
|   Assign of string * expr
|   Noexpr
|   ListP of expr list
|   DictP of (expr * expr) list
|   Call of string * expr list 
 let convert src - function
 	A.binop a -> Binop_Convert a *)

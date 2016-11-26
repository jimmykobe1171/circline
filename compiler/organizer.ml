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

let convert_edge_graph_list = function
  {A.graphs = g; A.edges = e} -> {C.graphs = convert_expr_list g; C.edges = convert_expr_list e}

let convert_formal = function
  | A.Formal(v, s) -> C.Formal(convert_var_type v, s)

let convert_formal_list = function
    [] -> []
  | [x] -> [convert_formal x]
  | _ as l -> (List.map convert_formal l)

(* create a main funcition outside of the whole statement list *)
let createMain stmts = A.Func({
    A.returnType = A.Int_t;
    A.name = "main";
    A.args = [];
    A.body = stmts;
  })

module StringMap = Map.Make(String)

let rec get_funcs_from_body_a = function
    [] -> []
  | A.Func(_) as x::tl -> x :: (get_funcs_from_body_a tl)
  | _::tl -> get_funcs_from_body_a tl

let rec get_body_from_body_a = function
    [] -> []
  | A.Func(_)::tl -> get_body_from_body_a tl
  | _ as x::tl -> x :: (get_body_from_body_a tl)

(* convert functions in A to a horizental function list in A *)
let rec convert_func_list m parent = function
    [] -> ([], m)
  | A.Func{A.returnType = r; A.name = n; A.args = a; A.body = b}::tl ->
    let m = StringMap.add n parent m in

    let addedFunc = A.Func({
      A.returnType = r; A.name = n; A.args = a; A.body = get_body_from_body_a b
    }) in
    let firstRes = convert_func_list m parent tl in
    let secondRes = convert_func_list (snd firstRes) n (get_funcs_from_body_a b) in
    ((addedFunc :: (fst firstRes) @ (fst secondRes)), (snd secondRes))
  | _::tl -> convert_func_list m parent tl

(* convert stament in A to C, except those Var_dec and Func, we will convert them separately *)
let rec convert_stmt = function
    A.Expr(a) -> C.Expr(convert_expr a)
  | A.Return(a) -> C.Return(convert_expr a)
  | A.For(e1, e2, e3, stls) -> C.For(convert_expr e1, convert_expr e2, convert_expr e3, List.map convert_stmt stls)
  | A.If(e, stls1, stls2) -> C.If(convert_expr e, List.map convert_stmt stls1, List.map convert_stmt stls2)
  | A.While(e, stls) -> C.While(convert_expr e, List.map convert_stmt stls)
  | _ -> C.Expr(C.Noexpr)

let rec get_body_from_body_c = function
    [] -> []
  | A.Var_dec(A.Local(_, name, v))::tl -> C.Expr(C.Assign(name, convert_expr v)) :: (get_body_from_body_c tl)
  | _ as x::tl -> (convert_stmt x) :: (get_body_from_body_c tl)

let rec get_local_from_body_c = function
    [] -> []
  | A.Var_dec(A.Local(typ, name, _))::tl -> C.Formal(convert_var_type typ, name) :: (get_local_from_body_c tl)
  | _::tl -> get_local_from_body_c tl

(* convert the horizental level function list in A to C *)
let rec convert_func_list_c m = function
    [] -> []
  | A.Func{A.returnType = r; A.name = n; A.args = a; A.body = b} :: tl -> {
    C.returnType = convert_var_type r;
    C.name = n;
    C.args = convert_formal_list a;
    C.body = get_body_from_body_c b;
    C.locals = get_local_from_body_c b;
    C.pname = (if n = "main" then "main" else StringMap.find n m)
  } :: (convert_func_list_c m tl)
  | _::tl -> convert_func_list_c m tl

(* entry point *)
let convert stmts =
  let funcs = createMain stmts in
  let horizen_funcs_m = convert_func_list StringMap.empty "main" [funcs] in
  convert_func_list_c (snd horizen_funcs_m) (fst horizen_funcs_m)
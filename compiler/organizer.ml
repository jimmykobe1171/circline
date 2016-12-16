module A = Ast
module C = Cast

module StringMap = Map.Make(String)
let node_num = ref 0

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
  | A.Greater -> C.Greater
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
  | A.List_Int_t -> C.List_Int_t
  | A.List_Float_t -> C.List_Float_t
  | A.List_String_t -> C.List_String_t
  | A.List_Node_t -> C.List_Node_t
  | A.List_Graph_t -> C.List_Graph_t
  | A.Dict_Int_t -> C.Dict_Int_t
  | A.Dict_Float_t -> C.Dict_Float_t
  | A.Dict_String_t -> C.Dict_String_t
  | A.Dict_Node_t -> C.Dict_Node_t
  | A.Dict_Graph_t -> C.Dict_Graph_t
  | A.Void_t -> C.Void_t
  | A.Null_t -> C.Null_t

let convert_graph_op = function
| A.Right_Link -> C.Right_Link
| A.Left_Link -> C.Left_Link
| A.Double_Link -> C.Double_Link

let rec get_entire_name m aux cur_name =
  if (StringMap.mem cur_name m) then
    let aux = (StringMap.find cur_name m) ^ "." ^ aux in
    (get_entire_name m aux (StringMap.find cur_name m))
  else aux

let increase_node_num =
  let node_num = ref(!node_num) in
  !(node_num) - 1

let rec convert_expr m = function
    A.Num_Lit(a) -> C.Num_Lit(convert_num a)
|   A.Null -> C.Null
|   A.String_Lit(a) -> C.String_Lit(a)
|   A.Bool_lit(a) -> C.Bool_lit(a)
|   A.Node(a) -> node_num := (!node_num + 1); C.Node(!node_num - 1, convert_expr m a)
|   A.Graph_Link(a,b,c,d) -> C.Graph_Link(
      convert_expr m a,
      convert_graph_op b,
      convert_expr m c,
      (match (c,d) with
        | (A.ListP(_), A.ListP(_))
        | (A.ListP(_), A.Noexpr)
        | (A.ListP(_), A.Null) -> convert_expr m d
        | (A.ListP(_), _) -> C.ListP([convert_expr m d])
        | _ -> convert_expr m d
      ))
|   A.Binop(a,b,c) -> C.Binop(convert_expr m a, convert_binop b, convert_expr m c)
|   A.Unop(a,b) -> C.Unop(convert_unop a, convert_expr m b)
|   A.Id(a) -> C.Id(a)
|   A.Assign(a,b) -> C.Assign(a, convert_expr m b)
|   A.Noexpr -> C.Noexpr
|   A.ListP(a) -> C.ListP(convert_expr_list m a)
|   A.DictP(a) -> C.DictP(convert_dict_list m a)
|   A.Call(a,b) -> C.Call(get_entire_name m a a, convert_expr_list m b)
|   A.CallDefault(a,b,c) -> C.CallDefault(convert_expr m a, b, convert_expr_list m c)

and convert_expr_list m = function
    [] -> []
  | [x] -> [convert_expr m x]
  | _ as l -> (List.map (convert_expr m) l)

and convert_dict m = function
  (c,d) -> (convert_expr m c, convert_expr m d)

and convert_dict_list m = function
    [] -> []
  | [x] -> [convert_dict m x]
  | _ as l -> (List.map (convert_dict m) l)

let convert_edge_graph_list m = function
  {A.graphs = g; A.edges = e} -> {C.graphs = convert_expr_list m g; C.edges = convert_expr_list m e}

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

let rec get_funcs_from_body_a = function
    [] -> []
  | A.Func(_) as x::tl -> x :: (get_funcs_from_body_a tl)
  | _::tl -> get_funcs_from_body_a tl

let rec get_body_from_body_a = function
    [] -> []
  | A.Func(_)::tl -> get_body_from_body_a tl
  | _ as x::tl -> x :: (get_body_from_body_a tl)

let rec mapper parent map = function
   [] -> map
 | A.Func{A.name = n; _}::tl ->
    mapper parent (StringMap.add n parent map) tl
 | _-> map

let convert_bfs_insider my_map = function
    A.Func{A.name = n; A.body = b; _}->
    let curr = get_funcs_from_body_a b in
      let my_map = mapper n my_map curr in
    (curr,my_map)
  | _->([],my_map)

let rec bfser m result = function
    [] ->(List.rev result, m)
  | A.Func{A.returnType = r; A.name = n; A.args = args; A.body = b} as a ::tl -> let result1 = convert_bfs_insider m a in
    let latterlist = tl @ (fst result1) in
    let m = (snd result1) in
    let addedFunc = A.Func({
      A.returnType = r; A.name = n; A.args = args; A.body = get_body_from_body_a b
    }) in
    let result = result @ [addedFunc] in
     bfser m result latterlist
  | _->([], m)

(* convert stament in A to C, except those Var_dec and Func, we will convert them separately *)
let rec convert_stmt m = function
    A.Expr(a) -> C.Expr(convert_expr m a)
  | A.Return(a) -> C.Return(convert_expr m a)
  | A.For(e1, e2, e3, stls) -> C.For(convert_expr m e1, convert_expr m e2, convert_expr m e3, List.map (convert_stmt m) stls)
  | A.If(e, stls1, stls2) -> C.If(convert_expr m e, List.map (convert_stmt m) stls1, List.map (convert_stmt m) stls2)
  | A.While(e, stls) -> C.While(convert_expr m e, List.map (convert_stmt m) stls)
  | _ -> C.Expr(C.Noexpr)


let rec get_body_from_body_c m = function
    [] -> []
  | A.Var_dec(A.Local(_, name, v))::tl when v <> A.Noexpr -> C.Expr(C.Assign(name, convert_expr m v)) :: (get_body_from_body_c m tl)
  | A.Var_dec(A.Local(_, _, v))::tl when v = A.Noexpr -> (get_body_from_body_c m tl)
  | _ as x::tl -> (convert_stmt m x) :: (get_body_from_body_c m tl)

let rec get_local_from_body_c = function
    [] -> []
  | A.Var_dec(A.Local(typ, name, _))::tl -> C.Formal(convert_var_type typ, name) :: (get_local_from_body_c tl)
  | _::tl -> get_local_from_body_c tl

(* convert the horizental level function list in A to C *)
let rec convert_func_list_c m = function
    [] -> []
  | A.Func{A.returnType = r; A.name = n; A.args = a; A.body = b} :: tl -> {
    C.returnType = convert_var_type r;
    C.name = get_entire_name m n n;
    C.args = convert_formal_list a;
    C.body = get_body_from_body_c m b;
    C.locals = get_local_from_body_c b;
    C.pname = if n = "main" then "main" else get_entire_name m (StringMap.find n m) (StringMap.find n m)
  } :: (convert_func_list_c m tl)
  | _::tl -> convert_func_list_c m tl

(* entry point *)
let convert stmts =
  let funcs = createMain stmts in
  let horizen_funcs_m = bfser StringMap.empty [] [funcs] in
  convert_func_list_c (snd horizen_funcs_m) (fst horizen_funcs_m)

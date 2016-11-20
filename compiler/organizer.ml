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
|   A.ListP(a) -> C.ListP(expr_looper [] a)
|   A.DictP(a) -> C.DictP(dict_looper [] a)
|   A.Call(a,b) -> C.Call(a, expr_looper [] b) 
|   A.CallDefault(a,b,c) -> C.CallDefault(convert_expr a, b, expr_looper [] c)

and expr_looper a = function
    [] -> []
  | s::ss -> (convert_expr s):: a

and dict_looper a = function
    [] -> []
  | (c,d)::ss -> (convert_expr c, convert_expr d)::a

let convert_stmt = function
  A.Expr(a) -> C.Expr(convert_expr a)


let rec convert curr_list = function
   [] -> curr_list
  | s::ss -> (convert_stmt s) :: (convert curr_list ss)




(*
(* Variable Type *)


let Convert_formal = function
  | A.Formal() -> C.Formal 

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

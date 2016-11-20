module A = Ast
module C = Cast
module P = Parserize

let Binop_Convert = function
  | A.Add -> C.Add
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
  | A.ListNodesAt -> C.ListNodeAt 
  | A.ListEdgesAt -> C.ListEdgesAt
  | A.RootAs -> C.RootAs

let Unop_Convert = function
  A.Neg -> C.Neg
| A.Not -> C.Not

let Num_Convert = function
  A.Num_Int -> C.Num_Int
| A.Num_Float -> C.Num_Float

(* Variable Type *)
let Convert_var_type = function
  A.Int_t -> C.Int_t                
| A.Float_t -> C.Float_t                
| A.String_t -> C.String_t
| A.Bool_t -> C.Bool_t
| A.Node_t -> C.Node_t
| A.Graph_t -> C.Graph_t
| A.List_t -> C.List_t
| A.Dict_Int_t -> C.Dict_Int_t
| A.Dict_Float_t -> C.Dict_Float_t
| A.Dict_String_t -> C.Dict_String_t
| A.Dict_Node_t -> C.Dict_Node_t
| A.Dict_Graph_t -> C.Dict_Graph_t
| A.Void_t -> C.Void_t
| A.Null_t -> C.Null_t

let Convert_formal = function
| A.Formal -> C.Formal

let Convert_graph_op = function
| A.Right_Link -> C.Right_Link
| A.Left_Link -> C.Left_Link
| A.Double_Link -> C.Double_Link

let Convert_expr =
    A.Num_Lit -> C.Num_Lit
|   A.Null -> C.Null
|   A.String_Lit -> C.String_Lit
|   Bool_lit of bool
|   Node of expr
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
 	A.binop a -> Binop_Convert a
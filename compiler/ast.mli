type binop =
  Add
| Sub
| Mult
| Div
| Equal
| Neq
| Less
| Leq
| Greater
| Geq
| And
| Or
| Mod

type unop =
  Sub
| Not

type num =
  Num_Int of int
| Num_Float of float

type expr =
    Num_Lit of num
| 	Binop of expr * binop * expr
|  	Unop of unop * expr
|   Id of string
|   Assign of string * expr

(* Statements *)
type stmt =
  Expr of expr     (* set foo = bar + 3 *)

(* Program entry point *)
type program = stmt list
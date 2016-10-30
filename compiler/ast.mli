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
(* |   Assign of string * expr *)

type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq | And | Not | Or | Mod

type expr =
		Int_Lit of int
	| 	Float_Lit of float
	| 	Binop of expr * op * expr
	|  	Unop of op * expr

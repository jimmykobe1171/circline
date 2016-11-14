open Ast

(* Pretty-printing functions *)
let string_of_typ = function
    Int_t -> "int"
  | Float_t -> "float"
  | String_t -> "string"
  | Bool_t -> "bool" 
  | Node_t -> "node" 
  | Graph_t -> "graph" 
  | List_t -> "list" 
  | Dict_Int_t -> "dic<int>" 
  | Dict_Float_t -> "dic<float>" 
  | Dict_String_t -> "dic<string>" 
  | Dict_Node_t -> "dic<node>" 
  | Dict_Graph_t -> "dic<graph>" 

let string_of_op = function
    Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Mod -> "%"
  | Equal -> "=="
  | Neq -> "!="
  | Less -> "<"
  | Leq -> "<="
  | Greater -> ">"
  | Geq -> ">="
  | And -> "and"
  | Or -> "or"
  | ListNodesAt -> "@"
  | ListEdgesAt -> "@@"
  | RootAs -> "~"

let string_of_uop = function
    Sub -> "-"
  | Not -> "!"

let string_of_formal = function
    Formal(var_type, var_name) -> string_of_typ var_type ^ " " ^ var_name

let rec string_of_expr = function
    Num_Lit(Num_Int(l)) -> string_of_int l
  | Num_Lit(Num_Float(l)) -> string_of_float l
  | String_Lit(l) -> l
  | Bool_lit(true) -> "true"
  | Bool_lit(false) -> "false"
  | Id(s) -> s
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Unop(o, e) -> string_of_uop o ^ string_of_expr e
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e
  | Node(e) -> "node(" ^ string_of_expr e ^ ")"
  (* | Type_Decl(formal, e) -> string_of_formal formal ^ " = " ^ string_of_expr e *)
  | Null -> "null"
  | Noexpr -> ""



module StringMap = Map.Make(String)

let check program = 
    (* loop through the stmt_list of program to collect global variable declarations *)
    let rec collect_globals list = function
        [] -> list
        | Local(typ, name, v) :: t -> (name, (typ, v)) :: collect_globals list t
        | _ :: t -> collect_globals list t
    in
    (* collect global variable declarations *)
    let globals = collect_globals [] program in

    (* Type of global variable *)
    let globals_symbols = List.fold_left (fun m (name, (typ, v)) -> StringMap.add name (typ, v) m)
        StringMap.empty globals
    in
    let type_of_identifier s =
      try fst (StringMap.find s globals_symbols)
      with Not_found -> raise (Failure ("undeclared identifier " ^ s))
    in
    (* Raise an exception of the given rvalue type cannot be assigned to
     the given lvalue type *)
    let check_assign lvaluet rvaluet err =
        if lvaluet == rvaluet then lvaluet else raise err
    in
    (* Raise an exception if the given list has a duplicate *)
    let report_duplicate exceptf list =
        let rec helper = function
            n1 :: n2 :: _ when n1 = n2 -> raise (Failure (exceptf n1))
            | _ :: t -> helper t
            | [] -> ()
        in helper (List.sort compare list)
    in
    (* Return the type of an expression or throw an exception *)
    let rec expr = function
        Num_Lit(Num_Int _) -> Int_t 
        | Num_Lit(Num_Float _) -> Float_t 
        | String_Lit _ -> String_t 
        | Bool_lit _ -> Bool_t
        | Id s -> type_of_identifier s
        | Binop(e1, op, e2) as e -> let t1 = expr e1 and t2 = expr e2 in
            (match op with
            (* +,-,*,/ *)
            Add | Sub | Mult | Div when t1 = Int_t && t2 = Int_t -> Int_t
            |Add | Sub | Mult | Div when t1 = Float_t && t2 = Float_t -> Float_t
            |Add | Sub | Mult | Div when t1 = Int_t && t2 = Float_t -> Float_t
            |Add | Sub | Mult | Div when t1 = Float_t && t2 = Int_t -> Float_t
            (* =, != *)
            | Equal | Neq when t1 = t2 -> Bool_t
            (* <, <=, >, >= *)
            | Less | Leq | Greater | Geq when (t1 = Int_t || t1 = Float_t) && (t2 = Int_t || t2 = Float_t) -> Bool_t
            (* and, or *)
            | And | Or when t1 = Bool_t && t2 = Bool_t -> Bool_t
            (* mode *)
            | Mod when t1 = Int_t && t2 = Int_t -> Int_t
            | _ -> raise (Failure ("illegal binary operator " ^
                      string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
                      string_of_typ t2 ^ " in " ^ string_of_expr e))
            )
        | Unop(op, e) as ex -> let t = expr e in
            (match op with
            Sub when t = Int_t -> Int_t
            |Sub when t = Float_t -> Float_t
            | Not when t = Bool_t -> Bool_t
            | _ -> raise (Failure ("illegal unary operator " ^ string_of_uop op ^
                    string_of_typ t ^ " in " ^ string_of_expr ex)))
        (* | Type_Decl(Formal(typ, name), e) as ex -> let lt = typ and rt = expr e in
            check_assign lt rt (Failure ("illegal assignment " ^ string_of_typ lt ^
            " = " ^ string_of_typ rt ^ " in " ^ string_of_expr ex)) *)
        | Assign(var, e) as ex -> let lt = type_of_identifier var and rt = expr e in
            check_assign lt rt (Failure ("illegal assignment " ^ string_of_typ lt ^
            " = " ^ string_of_typ rt ^ " in " ^ string_of_expr ex))
    in

    let stmt = function
        Expr(e) -> ignore (expr e)
        | Return e -> ignore (expr e)
    in

    let rec stmt_list = function
        Return _ :: _ -> raise (Failure "nothing may follow a return")
        | s::ss -> stmt s ; stmt_list ss
        | [] -> ()
    in
    (**** Checking duplicated Global Variables ****)
    report_duplicate (fun n -> "duplicate global " ^ n) (List.map fst globals);
    (**** Checking statements ****)
    stmt_list program;


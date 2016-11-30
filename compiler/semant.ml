open Cast
open Printf


module StringMap = Map.Make(String)

(* Pretty-printing functions *)
let string_of_typ = function
    Int_t -> "int"
  | Float_t -> "float"
  | String_t -> "string"
  | Bool_t -> "bool" 
  | Node_t -> "node" 
  | Graph_t -> "graph" 
  | List_Int_t -> "list<int>"
  | List_Float_t -> "list<float>"
  | List_String_t -> "list<string>"
  | List_Node_t -> "list<node>"
  | List_Graph_t -> "list<graph>"
  | Dict_Int_t -> "dic<int>" 
  | Dict_Float_t -> "dic<float>" 
  | Dict_String_t -> "dic<string>" 
  | Dict_Node_t -> "dic<node>" 
  | Dict_Graph_t -> "dic<graph>" 
  | Void_t -> "void"
  | Null_t -> "null"

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
    Neg -> "-"
  | Not -> "not"

let string_of_graph_op = function
    Right_Link -> "->"
  | Left_Link -> "<-"
  | Double_Link -> "--"

let rec string_of_expr = function
    Num_Lit(Num_Int(l)) -> string_of_int l
  | Num_Lit(Num_Float(l)) -> string_of_float l
  | Null -> "null"
  | String_Lit(l) -> l
  | Bool_lit(true) -> "true"
  | Bool_lit(false) -> "false"
  | Node(e) -> "node(" ^ string_of_expr e ^ ")"
  | Graph_Link(e1, op, e2, e3) -> 
      "graph_link(" ^ string_of_expr e1 ^ " " ^ string_of_graph_op op ^ " " ^ string_of_expr e2 ^ " " ^ string_of_expr e3 ^ ")"
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Unop(o, e) -> string_of_uop o ^ " " ^ string_of_expr e
  | Id(s) -> s
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e
  | Noexpr -> ""
  (* TODO: maybe revise to a more meaningful name *)
  | ListP(es) -> "list" 
  | DictP(es) -> "dict"
  | Call(n, es) -> "function call " ^ n
  | CallDefault(e, n, es) -> "function call " ^ string_of_expr e ^ "." ^ n
  

exception SemanticError of string

(* error message functions *)
let undeclared_function_error name =
    let msg = sprintf "undeclared function %s" name in
    raise (SemanticError msg)

let duplicate_formal_decl_error func name =
    let msg = sprintf "duplicate formal %s in %s" name func.name in
    raise (SemanticError msg)

let duplicate_local_decl_error func name =
    let msg = sprintf "duplicate local %s in %s" name func.name in
    raise (SemanticError msg)

let undeclared_identifier_error name =
    let msg = sprintf "undeclared identifier %s" name in
    raise (SemanticError msg)

let illegal_assignment_error lvaluet rvaluet ex =
    let msg = sprintf "illegal assignment %s = %s in %s" lvaluet rvaluet ex in
    raise (SemanticError msg)

let illegal_binary_operation_error typ1 typ2 op ex =
    let msg = sprintf "illegal binary operator %s %s %s in %s" typ1 op typ2 ex in
    raise (SemanticError msg)

let illegal_unary_operation_error typ op ex =
    let msg = sprintf "illegal unary operator %s %s in %s" op typ ex in
    raise (SemanticError msg)

let invaid_list_type_error typ = 
    let msg = sprintf "invalid list type: %s" typ in
    raise (SemanticError msg)

let invaid_dict_type_error typ = 
    let msg = sprintf "invalid dict type: %s" typ in
    raise (SemanticError msg)


let  match_list_type = function
  Int_t -> List_Int_t
| Float_t -> List_Float_t
| String_t -> List_String_t
| Node_t -> List_Node_t
| Graph_t -> List_Graph_t
| _ as t-> invaid_list_type_error (string_of_typ t)

let  match_dict_type = function
  Int_t -> Dict_Int_t
| Float_t -> Dict_Float_t
| String_t -> Dict_String_t
| Node_t -> Dict_Node_t
| Graph_t -> Dict_Graph_t
| _ as t-> invaid_dict_type_error (string_of_typ t)

let check_valid_list_type typ =
    if typ = List_Int_t || typ = List_Float_t || typ = List_String_t || typ = List_Node_t || typ = List_Graph_t then typ
    else invaid_list_type_error (string_of_typ typ)

let check_valid_dict_type typ =
    if typ = Dict_Int_t || typ = Dict_Float_t || typ = Dict_String_t || typ = Dict_Node_t || typ = Dict_Graph_t then typ
    else invaid_dict_type_error (string_of_typ typ)

(* get function obj from func_map, if not found, raise error *)
let get_func_obj name func_map = 
    try StringMap.find name func_map
    with Not_found -> undeclared_function_error name


(* Raise an exception if the given list has a duplicate *)
let report_duplicate exceptf list =
    let rec helper = function
        n1 :: n2 :: _ when n1 = n2 -> exceptf n1
        | _ :: t -> helper t
        | [] -> ()
    in helper (List.sort compare list)

(* check function *)
let check_function func_map func =
    (* check duplicate formals *)
    let args = List.map (fun (Formal(t, n)) -> n) func.args in
    report_duplicate (duplicate_formal_decl_error func) args;

    (* check duplicate locals *)
    let locals = List.map (fun (Formal(t, n)) -> n) func.locals in
    report_duplicate (duplicate_local_decl_error func) locals;

    
    (* search locally, if not found, then recursively search parent environment *)
    let rec type_of_identifier func s =
        let symbols = List.fold_left (fun m (Formal(t, n)) -> StringMap.add n t m)
            StringMap.empty (func.args @ func.locals )
        in
        try StringMap.find s symbols
        with Not_found ->
            if func.name = "main" then undeclared_identifier_error s else
            (* recursively search parent environment *)
            type_of_identifier (StringMap.find func.pname func_map) s
    in
    (* Raise an exception of the given rvalue type cannot be assigned to
    he given lvalue type, noted that int could be assinged to float type variable *)
    let check_assign lvaluet rvaluet ex = match lvaluet with
        Float_t when rvaluet = Int_t -> lvaluet
        | _ -> if lvaluet == rvaluet then lvaluet else 
            illegal_assignment_error (string_of_typ lvaluet) (string_of_typ rvaluet) (string_of_expr ex)
    in
    (* Return the type of an expression or throw an exception *)
    let rec expr = function
          Num_Lit(Num_Int _) -> Int_t 
        | Num_Lit(Num_Float _) -> Float_t
        | Null -> Null_t
        | String_Lit _ -> String_t 
        | Bool_lit _ -> Bool_t
        (* TODO: check node and graph *)
        | Node(e) -> Node_t
        | Graph_Link(e1, op, e2, e3) -> Graph_t
        | Binop(e1, op, e2) as e -> let t1 = expr e1 and t2 = expr e2 in
            (match op with
            (* +,-,*,/ *)
            Add | Sub | Mult | Div when t1 = Int_t && t2 = Int_t -> Int_t
            |Add | Sub | Mult | Div when t1 = Float_t && t2 = Float_t -> Float_t
            |Add | Sub | Mult | Div when t1 = Int_t && t2 = Float_t -> Float_t
            |Add | Sub | Mult | Div when t1 = Float_t && t2 = Int_t -> Float_t
            (* ==, != *)
            | Equal | Neq when t1 = t2 -> Bool_t
            (* <, <=, >, >= *)
            | Less | Leq | Greater | Geq when (t1 = Int_t || t1 = Float_t) && (t2 = Int_t || t2 = Float_t) -> Bool_t
            (* and, or *)
            | And | Or when t1 = Bool_t && t2 = Bool_t -> Bool_t
            (* mode *)
            | Mod when t1 = Int_t && t2 = Int_t -> Int_t
            | _ -> illegal_binary_operation_error (string_of_typ t1) (string_of_typ t2) (string_of_op op) (string_of_expr e)
            )
        | Unop(op, e) as ex -> let t = expr e in
            (match op with
            Neg when t = Int_t -> Int_t
            |Neg when t = Float_t -> Float_t
            | Not when t = Bool_t -> Bool_t
            | _ -> illegal_unary_operation_error (string_of_typ t) (string_of_uop op) (string_of_expr ex)
            )
        | Id s -> type_of_identifier func s
        (* check assignment operation, check cases of empty list and empty dict *)
        | Assign(var, ListP([])) -> let lt = type_of_identifier func var in check_valid_list_type lt
        | Assign(var, DictP([])) -> let lt = type_of_identifier func var in check_valid_dict_type lt
        | Assign(var, e) as ex -> let lt = type_of_identifier func var and rt = expr e in
            check_assign lt rt ex
        | Noexpr -> Void_t
        (* check list element type, if empty, assign List_Int_t here, and check again when encounter
        assignment operation *)
        | ListP([]) -> List_Int_t
        | ListP(es) -> 
            let element_type =
              let determine_element_type ss = List.fold_left 
                (fun l e -> (match l with
                  [] -> [expr e]
                | [t] when t = (expr e) -> [t]
                | _ -> raise (Failure ("list can not contain objects of different types")))
                ) [] ss
              in
              List.hd (determine_element_type es)
            in
            match_list_type element_type
        (* check dictionary element type if empty, assign Dict_Int_t here, and check again when encounter
        assignment operation *)
        | DictP([]) -> Dict_Int_t
        | DictP(es) ->
            let element_type =
              let determine_element_type ss = List.fold_left 
                (fun l (n, e) -> (match l with
                  [] -> [expr e]
                | [t] when t = (expr e) -> [t]
                | _ -> raise (Failure ("dict can not contain objects of different types")))
                ) [] ss
              in
              List.hd (determine_element_type es)
            in
            match_dict_type element_type
        | Call(n, args) -> let func_obj = get_func_obj n func_map in
              (* check function call such as the args length, args type *)
              let check_funciton_call func args =
                  let check_args_length l_arg r_arg = if (List.length l_arg) = (List.length r_arg)
                      then () else raise (Failure("args length not match in function call: " ^ func.name))
                  in
                  check_args_length func.args args;
                  (* l_arg is a list of Formal(typ, name), r_arg is a list of expr *)
                  let check_args_type l_arg r_arg =
                      List.iter2 
                          (fun (Formal(t, n)) r -> let r_typ = expr r in if t = r_typ then () else
                          raise (Failure("incompatible argument type " ^ string_of_typ r_typ ^ ", but " ^ string_of_typ t ^ " is expected"))) 
                          l_arg r_arg
                  in
                  (* do not check args type of function print, do conversion in codegen *)
                  if func.name = "print" then () else check_args_type func.args args
              in
              ignore(check_funciton_call func_obj args); func_obj.returnType
              (* TODO: implement call default *)
        (* |   CallDefault(e, n, es) -> *)
    in
    (* check statement *)
    let rec stmt = function
            Expr(e) -> ignore (expr e)
            | Return e -> ignore (expr e)
            | For(e1, e2, e3, stls) -> 
                ignore (expr e1); ignore (expr e2); ignore (expr e3); ignore(stmt_list stls)
            | If(e, stls1, stls2) -> ignore(e); ignore(stmt_list stls1); ignore(stmt_list stls2)
            | While(e, stls) -> ignore(e); ignore(stmt_list stls)
    and
    (* check statement list *)
    stmt_list = function
            Return _ :: ss when ss <> [] -> raise (Failure "nothing may follow a return")
            | s::ss -> stmt s ; stmt_list ss
            | [] -> ()

    in
    stmt_list func.body

(* program here is a list of functions *)
let check program =
    if List.mem "print" (List.map (fun f -> f.name) program)
        then raise (Failure ("function print may not be defined")) else ();
    (* TODO: check duplicate function *)

    (* Function declaration for a named function *)
    let built_in_funcs =  StringMap.add "print"
       { returnType = Void_t; name = "print"; args = [Formal(String_t, "x")];
         locals = []; body = []; pname = "main"} (StringMap.singleton "printb"
       { returnType = Void_t; name = "printb"; args = [Formal(Bool_t, "x")];
         locals = []; body = []; pname = "main"})
    in
    (* collect all functions and store in map with key=name, value=function *)
    let func_map = List.fold_left (fun m f -> StringMap.add f.name f m) built_in_funcs program in
    let check_function_wrapper func m =
        func m
    in
    (**** Checking functions ****)
    List.iter (check_function_wrapper check_function func_map) program

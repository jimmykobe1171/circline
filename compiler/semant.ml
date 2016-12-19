open Cast
open Printf
open Str


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
  | List_Bool_t -> "list<bool>"
  | List_Null_t -> "list<null>"
  | Dict_Int_t -> "dict<int>" 
  | Dict_Float_t -> "dict<float>" 
  | Dict_String_t -> "dict<string>" 
  | Dict_Node_t -> "dict<node>" 
  | Dict_Graph_t -> "dict<graph>" 
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
  | Node(_, e) -> "node(" ^ string_of_expr e ^ ")"
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

let inconsistent_list_element_type_error typ1 typ2 =
    let msg = sprintf "list can not contain objects of different types: %s and %s" typ1 typ2 in
    raise (SemanticError msg)

let inconsistent_dict_element_type_error typ1 typ2 =
    let msg = sprintf "dict can not contain objects of different types: %s and %s" typ1 typ2 in
    raise (SemanticError msg)

let unmatched_func_arg_len_error name =
    let msg = sprintf "args length not match in function call: %s" name in
    raise (SemanticError msg)

let incompatible_func_arg_type_error typ1 typ2 =
    let msg = sprintf "incompatible argument type %s, but %s is expected" typ1 typ2 in
    raise (SemanticError msg)

let invalid_expr_after_return_error _ =
    let msg = sprintf "nothing may follow a return" in
    raise (SemanticError msg)

let redefine_print_func_error _ =
    let msg = sprintf "function print may not be defined" in
    raise (SemanticError msg)

let duplicate_func_error name =
    let msg = sprintf "duplicate function declaration: %s" name in
    raise (SemanticError msg)

let unsupport_operation_error typ name =
    let msg = sprintf "unsupport operation on type %s: %s" typ name in
    raise (SemanticError msg)

let invalid_list_size_method_error ex =
    let msg = sprintf "list size method do not take arguments: %s" ex in
    raise (SemanticError msg)

let invalid_list_pop_method_error ex =
    let msg = sprintf "list pop method do not take arguments: %s" ex in
    raise (SemanticError msg)

let invalid_list_get_method_error ex =
    let msg = sprintf "list get method should only take one argument of type int: %s" ex in
    raise (SemanticError msg)

let invalid_list_add_method_error typ ex =
    let msg = sprintf "list add method should only take one argument of type %s: %s" typ ex in
    raise (SemanticError msg)

let invalid_list_push_method_error typ ex =
    let msg = sprintf "list push method should only take one argument of type %s: %s" typ ex in
    raise (SemanticError msg)

let invalid_list_remove_method_error ex =
    let msg = sprintf "list remove method should only take one argument of type int: %s" ex in
    raise (SemanticError msg)

let invalid_list_set_method_error typ ex =
    let msg = sprintf "list set method should only take two argument of type int and %s: %s" typ ex in
    raise (SemanticError msg)

let invalid_empty_list_decl_error ex =
    let msg = sprintf "invalid empty list declaration: %s" ex in
    raise (SemanticError msg)

let invalid_dict_get_method_error ex =
    let msg = sprintf "dict get method should only take one argument of type int, string or node: %s" ex in
    raise (SemanticError msg)

let invalid_dict_remove_method_error ex =
    let msg = sprintf "dict remove method should only take one argument of type int, string or node: %s" ex in
    raise (SemanticError msg)

let invalid_dict_size_method_error ex =
    let msg = sprintf "dict size method do not take arguments: %s" ex in
      raise (SemanticError msg)

let invalid_dict_keys_method_error ex =
    let msg = sprintf "dict keys method do not take arguments: %s" ex in
      raise (SemanticError msg)

let invalid_dict_put_method_error typ ex =
    let msg = sprintf "dict put method should only take two argument of type (int, string or node) and %s: %s" typ ex in
      raise (SemanticError msg)

let invalid_empty_dict_decl_error ex =
    let msg = sprintf "invalid empty dict declaration: %s" ex in
    raise (SemanticError msg)

let invalid_graph_root_method_error ex =
    let msg = sprintf "graph root method do not take arguments: %s" ex in
    raise (SemanticError msg)

let invalid_graph_size_method_error ex =
    let msg = sprintf "graph size method do not take arguments: %s" ex in
    raise (SemanticError msg)

let invalid_graph_nodes_method_error ex =
    let msg = sprintf "graph nodes method do not take arguments: %s" ex in
    raise (SemanticError msg)

let invalid_graph_edges_method_error ex =
    let msg = sprintf "graph edges method do not take arguments: %s" ex in
    raise (SemanticError msg)

let invalid_graph_link_error ex =
    let msg = sprintf "left side of graph link should be node type: %s" ex in
    raise (SemanticError msg)

let invalid_graph_edge_at_error ex =
    let msg = sprintf "invalid graph edge at: %s" ex in
    raise (SemanticError msg)

let invalid_graph_list_node_at_error ex =
    let msg = sprintf "invalid graph list node at: %s" ex in
    raise (SemanticError msg)

let unsupport_graph_list_edge_at_error ex =
    let msg = sprintf "unsupport graph list edge at: %s" ex in
    raise (SemanticError msg)

let invalid_graph_root_as_error ex =
    let msg = sprintf "invalid graph root as: %s" ex in
    raise (SemanticError msg)


let  match_list_type = function
  Int_t -> List_Int_t
| Float_t -> List_Float_t
| String_t -> List_String_t
| Node_t -> List_Node_t
| Graph_t -> List_Graph_t
| Bool_t -> List_Bool_t
| _ as t-> invaid_list_type_error (string_of_typ t)

let  reverse_match_list_type = function
  List_Int_t -> Int_t
| List_Float_t -> Float_t
| List_String_t -> String_t
| List_Node_t -> Node_t
| List_Graph_t -> Graph_t
| List_Bool_t -> Bool_t

let  match_dict_type = function
  Int_t -> Dict_Int_t
| Float_t -> Dict_Float_t
| String_t -> Dict_String_t
| Node_t -> Dict_Node_t
| Graph_t -> Dict_Graph_t
| _ as t-> invaid_dict_type_error (string_of_typ t)

let  reverse_match_dict_type = function
  Dict_Int_t -> Int_t
| Dict_Float_t ->  Float_t
| Dict_String_t -> String_t
| Dict_Node_t -> Node_t
| Dict_Graph_t -> Graph_t

(* list check helper function  *)
let check_valid_list_type typ =
    if typ = List_Int_t || typ = List_Float_t || typ = List_String_t || typ = List_Node_t || typ = List_Graph_t || typ = List_Bool_t then typ
    else invaid_list_type_error (string_of_typ typ)

let check_list_size_method ex es =
    match es with
    [] -> ()
    | _ -> invalid_list_size_method_error (string_of_expr ex)

let check_list_pop_method ex es =
    match es with
    [] -> ()
    | _ -> invalid_list_pop_method_error (string_of_expr ex)

(* dict check helper function  *)
let check_valid_dict_type typ =
    if typ = Dict_Int_t || typ = Dict_Float_t || typ = Dict_String_t || typ = Dict_Node_t || typ = Dict_Graph_t then typ
    else invaid_dict_type_error (string_of_typ typ)

let check_dict_size_method ex es =
    match es with
    [] -> ()
    | _ -> invalid_dict_size_method_error (string_of_expr ex)

let check_dict_keys_method ex es =
    match es with
    [] -> ()
    | _ -> invalid_dict_keys_method_error (string_of_expr ex)

(* graph check helper function  *)
let check_graph_root_method ex es =
    match es with
    [] -> ()
    | _ -> invalid_graph_root_method_error (string_of_expr ex)

let check_graph_size_method ex es =
    match es with
    [] -> ()
    | _ -> invalid_graph_size_method_error (string_of_expr ex)

let check_graph_nodes_method ex es =
    match es with
    [] -> ()
    | _ -> invalid_graph_nodes_method_error (string_of_expr ex)

let check_graph_edges_method ex es =
    match es with
    [] -> ()
    | _ -> invalid_graph_edges_method_error (string_of_expr ex)

let check_graph_list_node_at ex lt rt =
    if lt = Graph_t && rt = Node_t then () else
    invalid_graph_list_node_at_error (string_of_expr ex)

let check_graph_root_as ex lt rt =
    if lt = Graph_t && rt = Node_t then () else
    invalid_graph_root_as_error (string_of_expr ex)


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
        | Node_t when rvaluet = Null_t -> lvaluet
        | Graph_t when rvaluet = Null_t -> lvaluet
        | List_Int_t | List_String_t | List_Float_t | List_Node_t | List_Graph_t | List_Bool_t when rvaluet = Null_t -> lvaluet
        | Dict_Int_t | Dict_String_t | Dict_Float_t | Dict_Node_t | Dict_Graph_t when rvaluet = Null_t -> lvaluet
        | List_Int_t | List_String_t | List_Node_t when rvaluet = List_Null_t -> lvaluet
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
        | Node(_, e) -> Node_t
        | Graph_Link(e1, op, e2, e3) -> 
            let check_graph_link e1 op e2 e3 =
                let typ = expr e1 in
                match typ with
                Node_t -> ()
                |_ -> invalid_graph_link_error (string_of_expr e1)
            in
            ignore(check_graph_link e1 op e2 e3); Graph_t
        | EdgeAt(e, n1, n2) -> 
            let check_edge_at e n1 n2 =
                if (expr e) = Graph_t && (expr n1) = Node_t && (expr n2) = Node_t then ()
                else invalid_graph_edge_at_error (string_of_expr e)
            in
            ignore(check_edge_at e n1 n2); Edge_t
        | Binop(e1, op, e2) as e -> let t1 = expr e1 and t2 = expr e2 in
            (match op with
            (* +,-,*,/ *)
            Add | Sub | Mult | Div when t1 = Int_t && t2 = Int_t -> Int_t
            |Add | Sub | Mult | Div when t1 = Float_t && t2 = Float_t -> Float_t
            |Add | Sub | Mult | Div when t1 = Int_t && t2 = Float_t -> Float_t
            |Add | Sub | Mult | Div when t1 = Float_t && t2 = Int_t -> Float_t
            (* + - for graph *)
            | Add when t1 = Graph_t && t2 = Graph_t -> Graph_t
            | Sub when t1 = Graph_t && t2 = Graph_t -> List_Graph_t
            | Sub when t1 = Graph_t && t2 = Node_t -> List_Graph_t
            (* ==, != *)
            | Equal | Neq when t1 = t2 -> Bool_t
            (* <, <=, >, >= *)
            | Less | Leq | Greater | Geq when (t1 = Int_t || t1 = Float_t) && (t2 = Int_t || t2 = Float_t) -> Bool_t
            (* and, or *)
            | And | Or when t1 = Bool_t && t2 = Bool_t -> Bool_t
            (* mode *)
            | Mod when t1 = Int_t && t2 = Int_t -> Int_t
            | ListNodesAt -> ignore(check_graph_list_node_at e t1 t2); List_Node_t;
            | ListEdgesAt -> unsupport_graph_list_edge_at_error (string_of_expr e)
            | RootAs -> ignore(check_graph_root_as e t1 t2); Graph_t;
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
        | Assign(var, e) as ex -> let lt = type_of_identifier func var and rt = expr e in
            check_assign lt rt ex
        | Noexpr -> Void_t
        | ListP([]) as ex -> invalid_empty_list_decl_error (string_of_expr ex)
        | ListP(es) -> 
            let element_type =
              let determine_element_type ss = List.fold_left 
                (fun l e -> (match l with
                  [] -> [expr e]
                | [t] when t = (expr e) -> [t]
                | [t] when (t = Graph_t && (expr e) = Node_t) || (t = Node_t && (expr e) = Graph_t) -> [Graph_t]
                | [t] when (t = Float_t && (expr e) = Int_t) || (t = Int_t && (expr e) = Float_t) -> [Float_t]
                | [t] -> inconsistent_list_element_type_error (string_of_typ t) (string_of_typ (expr e))
                )) [] ss
              in
              List.hd (determine_element_type es)
            in
            match_list_type element_type
        | DictP([]) as ex -> invalid_empty_dict_decl_error (string_of_expr ex)
        | DictP(es) ->
            let element_type =
              let determine_element_type ss = List.fold_left 
                (fun l (n, e) -> (match l with
                  [] -> [expr e]
                | [t] when t = (expr e) -> [t]
                | [t] -> inconsistent_dict_element_type_error (string_of_typ t) (string_of_typ (expr e))
                )) [] ss
              in
              List.hd (determine_element_type es)
            in
            match_dict_type element_type
        | Call(n, args) -> let func_obj = get_func_obj n func_map in
              (* check function call such as the args length, args type *)
              let check_funciton_call func args =
                  let check_args_length l_arg r_arg = if (List.length l_arg) = (List.length r_arg)
                      then () else (unmatched_func_arg_len_error func.name)
                  in
                  if List.mem func.name ["printb"; "print"; "printf"; "string"; "float"; "int"; "bool"] then ()
                  else check_args_length func.args args;
                  (* l_arg is a list of Formal(typ, name), r_arg is a list of expr *)
                  let check_args_type l_arg r_arg =
                      List.iter2 
                          (fun (Formal(t, n)) r -> let r_typ = expr r in if t = r_typ then () else
                            incompatible_func_arg_type_error (string_of_typ r_typ) (string_of_typ t)
                          )
                          l_arg r_arg
                  in
                  (* do not check args type of function print, do conversion in codegen *)
                  if List.mem func.name ["printb"; "print"; "printf"; "string"; "float"; "int"; "bool"] then () 
                  else check_args_type func.args args
              in
              ignore(check_funciton_call func_obj args); func_obj.returnType
              (* TODO: implement call default *)
        | CallDefault(e, n, es) -> let typ = expr e in
              (* should not put it here, but we need function expr, so we cann't put outside *)
              let check_list_get_method ex es =
                  match es with
                  [x] when (expr x) = Int_t -> ()
                  | _ -> invalid_list_get_method_error (string_of_expr ex)
              in
              let check_list_add_method typ ex es =
                  match es with
                  [x] when (expr x) = (reverse_match_list_type typ) -> ()
                  | _ -> invalid_list_add_method_error (string_of_typ (reverse_match_list_type typ)) (string_of_expr ex)
              in
              let check_list_push_method typ ex es =
                  match es with
                  [x] when (expr x) = (reverse_match_list_type typ) -> ()
                  | _ -> invalid_list_push_method_error (string_of_typ (reverse_match_list_type typ)) (string_of_expr ex)
              in
              let check_list_remove_method ex es =
                  match es with
                  [x] when (expr x) = Int_t -> ()
                  | _ -> invalid_list_remove_method_error (string_of_expr ex)
              in
              let check_list_set_method typ ex es =
                  match es with
                  [index; value] when (expr index) = Int_t && (expr value) = (reverse_match_list_type typ) -> ()
                  | _ -> invalid_list_set_method_error (string_of_typ (reverse_match_list_type typ)) (string_of_expr ex)
              in
              let check_dict_get_method ex es =
                  match es with
                  [x] when List.mem (expr x) [Int_t; String_t; Node_t] -> ()
                  | _ -> invalid_dict_get_method_error (string_of_expr ex)
              in
              let check_dict_remove_method ex es =
                  match es with
                  [x] when List.mem (expr x) [Int_t; String_t; Node_t] -> ()
                  | _ -> invalid_dict_remove_method_error (string_of_expr ex)
              in
              let check_dict_put_method typ ex es =
                  match es with
                  [key; value] when List.mem (expr key) [Int_t; String_t; Node_t] 
                                    && ((expr value) = (reverse_match_dict_type typ) || (expr value) = Null_t) -> ()
                  | _ -> invalid_dict_put_method_error (string_of_typ (reverse_match_dict_type typ)) (string_of_expr ex)
              in
              match typ with
                  List_Int_t | List_Float_t | List_String_t | List_Node_t | List_Graph_t | List_Bool_t -> 
                    (match n with
                      "add" -> ignore(check_list_add_method typ e es); typ
                      | "push"  -> ignore(check_list_push_method typ e es); typ
                      | "remove" -> ignore(check_list_remove_method e es); typ
                      | "set" -> ignore(check_list_set_method typ e es); typ
                      (* | "concat" ->  *)
                      | "pop" -> ignore(check_list_pop_method e es); reverse_match_list_type typ
                      | "get" -> ignore(check_list_get_method e es); reverse_match_list_type typ
                      | "size" -> ignore(check_list_size_method e es); Int_t
                      | _ -> unsupport_operation_error (string_of_typ typ) n
                    )
                  | Dict_Int_t | Dict_Float_t | Dict_String_t | Dict_Node_t | Dict_Graph_t ->
                    (* key support type node, string, int *)
                    (match n with
                      "put" -> ignore(check_dict_put_method typ e es); typ
                      | "get" -> ignore(check_dict_get_method e es); reverse_match_dict_type typ
                      | "remove" -> ignore(check_dict_remove_method e es); typ
                      | "size" -> ignore(check_dict_size_method e es); Int_t
                      (* return List_Null_t here to bypass the semantic check *)
                      | "keys" -> ignore(check_dict_keys_method e es); List_Null_t
                      | _ -> unsupport_operation_error (string_of_typ typ) n
                    )
                  | Graph_t ->
                    (match n with
                      "root" -> ignore(check_graph_root_method e es); Node_t
                      | "size" -> ignore(check_graph_size_method e es); Int_t
                      | "nodes" -> ignore(check_graph_nodes_method e es); List_Node_t
                      | "edges" -> ignore(check_graph_edges_method e es); List_Int_t
                    )
                  | _ -> unsupport_operation_error (string_of_typ typ) n
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
            Return _ :: ss when ss <> [] -> invalid_expr_after_return_error ss
            | s::ss -> stmt s ; stmt_list ss
            | [] -> ()

    in
    stmt_list func.body

(* program here is a list of functions *)
let check program =
    let end_with s1 s2 =
        let len1 = String.length s1 and len2 = String.length s2 in
        if len1 < len2 then false
        else 
            let last = String.sub s1 (len1-len2) len2 in 
            if last = s2 then true else false
    in
    if List.mem true (List.map (fun f -> end_with f.name "print") program)
    then redefine_print_func_error "_" else ();
    (* check duplicate function *)
    let m = StringMap.empty in
    List.map (fun f ->
        if StringMap.mem f.name m 
        then (duplicate_func_error f.name)
        else StringMap.add f.name true m) program;
    (* Function declaration for a named function *)
    let built_in_funcs =
      let funcs = [
          (
            "print",
           { returnType = Void_t; name = "print"; args = [Formal(String_t, "x")];
             locals = []; body = []; pname = "main"}
          );
          (
            "printb",
           { returnType = Void_t; name = "printb"; args = [Formal(Bool_t, "x")];
             locals = []; body = []; pname = "main"}
          );
          (
          "printf",
           { returnType = Void_t; name = "printf"; args = [Formal(String_t, "x")];
             locals = []; body = []; pname = "main"}
          );
          (
          "string",
           { returnType = String_t; name = "string"; args = [Formal(String_t, "x")];
             locals = []; body = []; pname = "main"}
          );
          (
          "int",
           { returnType = Int_t; name = "int"; args = [Formal(String_t, "x")];
             locals = []; body = []; pname = "main"}
          );
          (
          "float",
           { returnType = Float_t; name = "float"; args = [Formal(String_t, "x")];
             locals = []; body = []; pname = "main"}
          );
          (
          "bool",
           { returnType = Bool_t; name = "bool"; args = [Formal(String_t, "x")];
             locals = []; body = []; pname = "main"}
          )
      ]
      in
      let add_func funcs m =
          List.fold_left (fun m (n, func) -> StringMap.add n func m) m funcs
      in
      add_func funcs StringMap.empty
    in
    (* collect all functions and store in map with key=name, value=function *)
    let func_map = List.fold_left (fun m f -> StringMap.add f.name f m) built_in_funcs program in
    let check_function_wrapper func m =
        func m
    in
    (**** Checking functions ****)
    List.iter (check_function_wrapper check_function func_map) program

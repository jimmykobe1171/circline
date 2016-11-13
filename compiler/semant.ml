open Ast

module StringMap = Map.Make(String)

let check program = 
    (* loop through the stmt_list of program to collect global variable declarations *)
    let rec collect_globals list = function
        [] -> list
        | Expr(Type_Decl((Formal(typ, name)), e)) :: t -> (typ, name) :: collect_globals list t
        | _ :: t -> collect_globals list t
    in
    (* collect global variable declarations *)
    let globals = collect_globals [] program in

    (* Raise an exception if the given list has a duplicate *)
    let report_duplicate exceptf list =
        let rec helper = function
            n1 :: n2 :: _ when n1 = n2 -> raise (Failure (exceptf n1))
            | _ :: t -> helper t
            | [] -> ()
        in helper (List.sort compare list)
    in
    (**** Checking Global Variables ****)
    report_duplicate (fun n -> "duplicate global " ^ n) (List.map snd globals);
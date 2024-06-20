(* open Notty  *)
open Notty_unix 

type cursor_pos_type = { mutable x : int; mutable y: int}

let term = Term.create() 
let cursor_pos = {x=0; y=0}

let get_cpos() = Some (cursor_pos.x, cursor_pos.y)

let rec main_loop term = 
    cursor_pos.x <- 0; cursor_pos.y <- 0;       (* delete this later, currently needed as it cries about it not being mutable *)
    Term.cursor term (get_cpos());
    match Term.event term with 
    | `End | `Key (`Escape, []) -> ()
    | _ -> main_loop term

let _ = main_loop term 


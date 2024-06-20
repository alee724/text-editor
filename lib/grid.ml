module type GridMod = sig 
    type elem 
    type grid 

    val empty : unit -> grid 
    val add_row : ?row:int -> grid -> unit 
    val remove_row : ?row:int -> grid -> unit 
    val get_row : ?row:int -> grid -> elem list ref 
    val toString : grid -> string 
    val optimize : elem list ref -> unit 
    val deoptimize : elem list ref -> unit 
end

module Grid : GridMod with type elem = string = struct 
    type elem = string 
    type grid = elem list ref list ref 

    let empty () = ref [ ref [] ]

    let add_row ?(row=(-1)) grid = 
        let rec helper row grid = 
            match grid with 
            | [] -> !(empty())
            | _ when row <= 0 -> List.append grid !(empty())
            | h::t -> h :: helper (row - 1) t 
        in
        grid := helper row !grid 

    let remove_row ?(row=(-1)) grid = 
        let rec helper row grid = 
            match grid with 
            | [] -> []
            | [_] when row = -1 -> []
            | _::t when row = 0 -> t 
            | h::t when row = -1 -> h::helper row t 
            | h::t -> h:: helper (row-1) t
        in
        grid := helper row !grid 

    let get_row ?(row=(-1)) grid = 
        let rec helper row grid = 
            match grid with 
            | [] -> ref []
            | [x] when row = -1 -> x 
            | h::_ when row = 0 -> h
            | _::t when row = -1 -> helper row t 
            | _::t -> helper (row - 1) t 
        in
        helper row !grid 

    let str_concat elem acc = acc ^ elem

    let toString grid = 
        let rec helper grid =
            match grid with 
            | [] -> ""
            | h::t -> List.fold_right str_concat !h "" ^ "\n" ^ helper t 
        in
        helper !grid 

    let optimize lst_ref = lst_ref := [List.fold_right str_concat !lst_ref ""]

    let str_to_lst s acc =
        s |> String.to_seq |> List.of_seq |> List.map (String.make 1) |> List.append acc 

    let deoptimize lst_ref = 
        lst_ref := List.fold_right str_to_lst !lst_ref []
end

open Notty

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
  val image : grid -> I.t
  val square : ?row:int -> grid -> int -> unit
  val length : grid -> int
  val from_string : grid -> string -> unit
end

module Grid : GridMod with type elem = string = struct
  type elem = string
  type grid = elem list ref list ref

  let empty () = ref [ ref [] ]

  let add_row ?(row = -1) grid =
    let rec helper row grid =
      match grid with
      | [] -> !(empty ())
      | _ when row <= 0 -> List.append grid !(empty ())
      | h :: t -> h :: helper (row - 1) t
    in
    grid := helper row !grid

  let remove_row ?(row = -1) grid =
    let rec helper row grid =
      match grid with
      | [] -> []
      | [ _ ] when row = -1 -> []
      | _ :: t when row = 0 -> t
      | h :: t when row = -1 -> h :: helper row t
      | h :: t -> h :: helper (row - 1) t
    in
    grid := helper row !grid

  let get_row ?(row = -1) grid =
    let rec helper row grid =
      match grid with
      | [] -> ref []
      | [ x ] when row = -1 -> x
      | h :: _ when row = 0 -> h
      | _ :: t when row = -1 -> helper row t
      | _ :: t -> helper (row - 1) t
    in
    helper row !grid

  let str_concat acc elem = acc ^ elem

  let toString grid =
    let rec helper grid =
      match grid with
      | [] -> ""
      | [ x ] -> List.fold_left str_concat "" !x
      | h :: t -> List.fold_left str_concat "" !h ^ "\n" ^ helper t
    in
    helper !grid

  let optimize lst_ref = lst_ref := [ List.fold_left str_concat "" !lst_ref ]

  let str_to_lst acc s =
    s |> String.to_seq |> List.of_seq
    |> List.map (String.make 1)
    |> List.append acc

  let deoptimize lst_ref = lst_ref := List.fold_left str_to_lst [] !lst_ref

  let image grid =
    let rec inner_helper line =
      match line with
      | [] -> I.void 0 0
      | h :: t -> I.(string A.empty h <|> inner_helper t)
    in
    let rec out_helper grid =
      match grid with
      | [] -> I.void 0 1
      | h :: t -> I.(inner_helper !h <-> out_helper t)
    in
    out_helper !grid

  let reassign lst length =
    let rec helper lst length =
      match lst with
      | [] -> ([], [])
      | h :: t when length = 1 -> ([ h ], t)
      | h :: t ->
          let spl = helper t (length - 1) in
          (h :: fst spl, snd spl)
    in
    let new_lst = helper !lst length in
    lst := fst new_lst;
    snd new_lst

  let square ?(row = 0) grid length =
    let rec outer row dr_grid =
      match dr_grid with
      | [] -> ()
      | h :: t when row = 0 -> (
          deoptimize h;
          match List.length !h < length with
          | true ->
              optimize h;
              outer 0 t
          | false -> (
              let excess = reassign h length in
              match t with
              | [] ->
                  add_row grid;
                  let last_row = get_row grid in
                  last_row := excess
              | h2 :: _ ->
                  h2 := List.append excess !h2;
                  optimize h;
                  outer 0 t))
      | _ :: t -> outer (row - 1) t
    in
    outer row !grid

  let length grid = List.length !grid

  let from_string grid str =
    let lst = Str.split (Str.regexp "[\n]+") str in
    let lst = List.map (fun x -> ref [ x ]) lst in
    grid := lst
end

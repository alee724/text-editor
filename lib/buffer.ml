open Grid

module type BuffMod = sig
  type buff
  type grid

  val empty : unit -> buff
  val add : buff -> Grid.elem -> unit
  val ch_buff : grid -> buff -> int -> unit
  val left_to_right : ?num:int -> buff -> unit
  val right_to_left : ?num:int -> buff -> unit
  val delete : ?num:int -> buff -> unit
  val get_line : buff -> Grid.elem list ref
  val get_left : buff -> Grid.elem list
  val get_right : buff -> Grid.elem list
  val set : buff -> Grid.elem list -> Grid.elem list -> unit
  val update_buff : buff -> unit
  val set_line : buff -> Grid.elem list ref -> unit 
end

module Buff : BuffMod with type grid = Grid.grid = struct
  type buff = {
    mutable left : Grid.elem list;
    mutable right : Grid.elem list;
    mutable line : Grid.elem list ref;
  }

  type grid = Grid.grid

  let empty () = { left = []; right = []; line = ref [] }
  let update buff = buff.line := List.append buff.left buff.right

  let add buff elem =
    buff.left <- List.append buff.left [ elem ];
    update buff

  let ch_buff grid buff row =
    update buff;
    Grid.optimize buff.line;
    let line = Grid.get_row ~row grid in
    Grid.deoptimize line;
    buff.left <- [];
    buff.right <- !line;
    buff.line <- line

  let rec split lst len =
    match lst with
    | [] -> ([], [])
    | x when len <= 0 -> ([], x)
    | h :: t ->
        let s = split t (len - 1) in
        (h :: fst s, snd s)

  let left_to_right ?(num = 1) buff =
    let n = List.length buff.left - num in
    let spl = split buff.left n in
    buff.left <- fst spl;
    buff.right <- List.append (snd spl) buff.right

  let right_to_left ?(num = 1) buff =
    let spl = split buff.right num in
    buff.left <- List.append buff.left (fst spl);
    buff.right <- snd spl

  let delete ?(num = 1) buff =
    let rec helper lst num =
      match lst with
      | [] -> []
      | x when num = 0 -> x
      | _ :: t -> helper t (num - 1)
    in
    let n_lst = helper (List.rev buff.left) num in
    buff.left <- List.rev n_lst;
    update buff

  let get_line buff = buff.line
  let get_left buff = buff.left
  let get_right buff = buff.right

  let set buff left right =
    buff.left <- left;
    buff.right <- right;
    update buff

  let update_buff buff =
    let len = List.length buff.left in
    Grid.deoptimize buff.line;
    buff.left <- [];
    buff.right <- !(buff.line);
    right_to_left ~num:len buff

  let set_line buff line =
    buff.line <- line;
    update_buff buff
end

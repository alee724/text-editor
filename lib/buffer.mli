(* Outline of what needs to be done  *)
(* - the basic split buffer feature of this module  *)
(* - needs to have some form of "grid" such that we can have multiple lines *)
(* - functions revealed to the "client" will only involve methods that already optimize or deoptimize a line  *)
(* - no need to care about colors for now, not sure if I can easily implement that as of now *)
(* - needs functions for changing lines, adding to a line, moving elements from one buffer to another  *)
(* - creation of a new line, deleting a line, wrap around, pushing forward *)
(* - that is all that pops up in mind for now *)

open Grid

module type BuffMod = sig
  type buff
  (** The representation of the split buffer, consists of a left buffer, 
        a right buffer, and the line it uses to make the split buffer  *)

  type grid
  (* The representation of a grid taken from the grid module *)

  val empty : unit -> buff
  (* [empty] is the empty buffer *)

  val add : buff -> Grid.elem -> unit
  (* [add buff e] adds an element [e] to [buff] by appending the element to the left buffer *)

  val ch_buff : grid -> buff -> int -> unit
  (** [ch_buff grid buff n] changes the buffer buff to the [n] row in [grid], 
        this function automatically optimizes the previous row and deoptimizes the new row  *)

  val left_to_right : ?num:int -> buff -> unit
  (** [left_to_right ?num buff] takes n elements from the left buffer and moves it to the 
        right buffer in [buff], n is 1 if not specified by [num] *)

  val right_to_left : ?num:int -> buff -> unit
  (** [right_to_left ?num buff] takes n elements from the right buffer and moves it to the 
        left buffer in [buff], n is 1 if not specified by [num] *)

  val delete : ?num:int -> buff -> unit
  (** [delete num buff] deletes n elements from the left buffer in [buff], 
        n is 1 if not specified by [num] *)
end

module Buff : BuffMod with type grid = Grid.grid

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

  val get_line : buff -> Grid.elem list ref
  (** [get_line buff] returns the referenced line in [buff] *)

  val get_left : buff -> Grid.elem list 
  (* [get_left b] obtains the contents of the left buffer, 
     i.e. everything to the left of the buffer *)

  val get_right : buff -> Grid.elem list 
  (* [get_right b] obtains the contents of the right buffer, 
     i.e. everything to the right of the buffer *)

  val set : buff -> Grid.elem list -> Grid.elem list -> unit 
  (* [set buff left right] sets the contents of the left and right buffers 
     of [buff] with [left] and [right] *)

  val update_buff : buff -> unit
  (** [update_buff b] updates the buffer [b] with the contents on the line. This should only 
      be used if changes were made to the grid containing the line but the buffer has not been 
      changed accordingly *)
end

module Buff : BuffMod with type grid = Grid.grid

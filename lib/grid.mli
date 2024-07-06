open Notty

module type GridMod = sig
  type elem
  (** [elem] is the type of the element that will be in the grid *)

  type grid
  (** [grid] is the grid of elements *)

  val empty : unit -> grid
  (** [empty] is the empty grid *)

  val add_row : ?row:int -> grid -> unit
  (** [add_row ?row grid] adds an empty row to [grid]. 
        The row is appended at the end unless specified otherwise by [row] *)

  val remove_row : ?row:int -> grid -> unit
  (** [remove_row ?row grid] removes a row from [grid]. 
        The row is removed from the end unless specified otherwise by [row] *)

  val get_row : ?row:int -> grid -> elem list ref
  (** [get_row ?row grid] returns a row reference from [grid]. 
        The row is obtained from the end unless specified otherwise by [row] *)

  val toString : grid -> string
  (** [toString grid] converts [grid] to a string representation *)

  val optimize : elem list ref -> unit
  (** [optimize lst] optimizes the list reference [lst] by 
        combining common elements according to some logic *)

  val deoptimize : elem list ref -> unit
  (** [deoptimize lst] deoptimized the list reference [lst] by tokenizing 
        the list into smallest possible parts according to some logic *)

  val image : grid -> I.t
  (** [image grid] creates an image from [grid] to be used in the notty terminal *)

  val square : ?row:int -> grid -> int -> unit
  (** [square r grid l] makes [grid] square according to the length [l] starting from 
      row [r], pushing excess contents to the next row *)

  val length : grid -> int
  (** [length grid] does the List.length operation of the grid to get the total 
      number of rows in the grid *)

  val from_string : grid -> string -> unit 
  (** [from_string o s] replaces the grid [o] with a new grid made from a string [s] *)
end

module Grid : GridMod with type elem = string

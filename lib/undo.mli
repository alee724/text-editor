type 'a node = {
  mutable prev : 'a node option;
  value : 'a;
  mutable next : 'a node option;
  cursor_pos : int * int;
}

module type UndoMod = sig
  type elem
  (** [elem] is the element of the undo list *)

  type nlist
  (** An ['a nlist] is a nlist of a mutable doubly-linked list. It contains a value
    of type ['a] and a link to the [next] and [prev] nlist. As well as the coordinates to 
    the cursor position at the time *)

  val empty : unit -> nlist
  (** [empty] is an empty nlist *)

  val add : nlist -> elem -> int * int -> unit
  (** [add n e] adds an element [e] as the next element *)

  val undo : nlist -> unit
  (** [undo n] traverses back the list, setting the nlist [n] as the previous nlist if possible *)

  val redo : nlist -> unit
  (** [redo n] traverse forward in the list, setting the nlist [n] as the next nlist *)

  val getPos : nlist -> int * int
  (** [getGlst lst] gets the position of the cursor [(x,y)] of the current node in [lst] *)

  val getVal : nlist -> elem
  (** [getVal nlist] returns the current value stored in the nlist *)
end

module Undo : UndoMod with type elem = string

type 'a node = {
  mutable prev : 'a node option;
  value : 'a;
  mutable next : 'a node option;
  cursor_pos : int * int;
}

module type UndoMod = sig
  type elem
  type nlist

  val empty : unit -> nlist
  val add : nlist -> elem -> int * int -> unit
  val undo : nlist -> unit
  val redo : nlist -> unit
  val getPos : nlist -> int * int
  val getVal : nlist -> elem
end

module Undo : UndoMod with type elem = string = struct
  type elem = string
  type nlist = elem node ref

  let empty () =
    let lst =
        { prev = None; value = ""; next = None; cursor_pos = (0, 0) }
    in
    ref lst

  let add lst e cord =
    let next =
      { prev = Some !lst; value = e; next = None; cursor_pos = cord }
    in
    !lst.next <- Some next;
    lst := next

  let undo lst = match !lst.prev with None -> () | Some x -> lst := x
  let redo lst = match !lst.next with None -> () | Some x -> lst := x
  let getPos lst = !lst.cursor_pos
  let getVal lst = !lst.value
end

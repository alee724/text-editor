open Notty
open Notty_unix
open Text.Buffer
open Text.Grid

type cursor_pos_type = { mutable x : int; mutable y : int }

let term = Term.create ()
let cursor_pos = { x = 0; y = 0 }
let dim = Term.size term
let grid = Grid.empty ()
let buff = Buff.empty ();;

Buff.ch_buff grid buff 0

let move_to x y =
  cursor_pos.x <- x;
  cursor_pos.y <- y

let move_by x y =
  cursor_pos.x <- cursor_pos.x + x;
  cursor_pos.y <- cursor_pos.y + y

let typing num_keys =
  move_by num_keys 0;
  match cursor_pos.x >= fst dim with
  | true ->
      move_to 0 (cursor_pos.y + 1);
      Grid.add_row ~row:cursor_pos.y grid;
      Buff.ch_buff grid buff cursor_pos.y
  | false -> (
      match cursor_pos.x < 0, cursor_pos.y > 0 with
      | true, true ->
          move_by 0 (-1);
          Buff.ch_buff grid buff cursor_pos.y;
          let line_len = List.length !(Grid.get_row ~row:cursor_pos.y grid) in
          Buff.right_to_left ~num:line_len buff;
          move_to (line_len-1) cursor_pos.y
      | true, false -> move_to 0 0
      | _, _ -> ())

let special_event ev =
  match ev with
  | `Arrow dir -> (
      match dir with
      | `Up when cursor_pos.y > 0 -> move_by 0 (-1)
      | `Down when cursor_pos.y < snd dim -> move_by 0 1
      | `Right when cursor_pos.x < fst dim -> move_by 1 0
      | `Left when cursor_pos.x > 0 -> move_by (-1) 0
      | _ -> ())
  | `Tab ->
      typing 4;
      Buff.add buff "    "
      (* consider making a simple parser and tokenizer to make a proper tabbing feature *)
  | `Backspace | `Delete ->
          typing (-1);
      Buff.delete buff
  | _ -> ()

let key_presses key mods =
  match (key, mods) with
  | x, [] ->
      Buff.add buff (String.make 1 x);
      typing 1
  | _ -> ()

let rec main_loop term =
  Term.image term (Grid.image grid);
  Term.cursor term (Some (cursor_pos.x, cursor_pos.y));
  match Term.event term with
  | `End | `Key (`Escape, []) -> ()
  | ev ->
      (match ev with
      | `Key ((#Unescape.special as sp), _) -> special_event sp
      | `Key (`ASCII ch, mods) -> key_presses ch mods
      | _ -> ());
      main_loop term

let _ = main_loop term

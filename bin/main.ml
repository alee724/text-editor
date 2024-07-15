open Notty
open Notty_unix
open Text.Buffer
open Text.Grid
open Text.Undo
open Text.File

(* [grid_save n g] saves the grid [g] with a name [n], if no such file
   exists then it makes one *)
let grid_save name grid = save_file name (Grid.toString grid)

(* [grid_load n g] loads the data in the file given by a path [n] into the grid [g] *)
let grid_load name grid = Grid.from_string grid (load_file name)

(* [get_time] is a simple method utilizing the Core.Time_ns module to get the current
   time in NY, any other time zone would need slight modifications *)
let get_time () =
  String.sub
    Core.Time_ns.(to_string_utc (sub (now ()) (Span.create ~hr:4 ())))
    0 19

type cursor_pos_type = { mutable x : int; mutable y : int }
(** [cursor_pos_type] is the type for the cursor postiton on the screen. Keeps in track of 
    the x and y coordinates of the cursor *)

(* initializing some variables used for the terminal *)
let term = Term.create ()
let cursor_pos = { x = 0; y = 0 }
let dim = Term.size term
let grid = Grid.empty ()

(* initializing variables for a command line *)
let cmd_line = ref []
let cmd_buff = Buff.empty ()
let cmd_cusor = { x = 0; y = snd dim - 1 }
let cmd_mode = ref false;;

grid_load Sys.argv.(1) grid

let buff = Buff.empty ()
let undo = Undo.empty ();;

Buff.ch_buff grid buff 0

(* [move_to x y] moves the cursor position to [x] and [y]
    Can not move beyond the boundaries of the terminal, e.g. (-1, -1) *)
let move_to x y =
  cursor_pos.x <- x;
  cursor_pos.y <- y

(* [move_by x y] moves the cursor position by [x] and [y] units as it would on a coordinate graph
   Can not move beyond the boundaries of the terminal, e.g. (-1, -1) *)
let move_by x y =
  cursor_pos.x <- cursor_pos.x + x;
  cursor_pos.y <- cursor_pos.y + y

(* [check] checks the current length of the buffer and if the buffer length exceeds the
   length of the horizontal dimensions of the terminal then it calls on [Grid.square]
   to make the grid square to the width of the terminal *)
let check () =
  match List.length !(Buff.get_line buff) < fst dim with
  | true -> ()
  | false ->
      Grid.square ~row:cursor_pos.y grid (fst dim);
      Buff.update_buff buff

(* [delete] deletes the character located before the cursor and moves back a line if the
   cursor was at the leftmost position *)
let delete () =
  move_by (-1) 0;
  (match (cursor_pos.x < 0, cursor_pos.y > 0) with
  | true, true ->
      move_by 0 (-1);
      Buff.ch_buff grid buff cursor_pos.y;
      let line_len = List.length !(Grid.get_row ~row:cursor_pos.y grid) in
      Buff.right_to_left ~num:line_len buff;
      move_to (line_len - 1) cursor_pos.y
  | true, false -> move_to 0 0
  | _, _ -> ());
  Buff.delete buff

(* [typing num] is a method that gets called when typing [num] characters at once and
   wrapsaround the cursor if needed *)
let typing num_keys =
  move_by num_keys 0;
  (match cursor_pos.x >= fst dim with
  | true ->
      move_to 0 (cursor_pos.y + 1);
      (match Grid.length grid <= cursor_pos.y with
      | false -> ()
      | true -> Grid.add_row ~row:cursor_pos.y grid);
      Buff.ch_buff grid buff cursor_pos.y
  | false -> ());
  Undo.add undo (Grid.toString grid) (cursor_pos.x, cursor_pos.y)

(* [change_line] is called on whenever [cursor_pos.y] changed coordinates and thus
   changing the line *)
let change_line () =
  Buff.ch_buff grid buff cursor_pos.y;
  Buff.right_to_left ~num:cursor_pos.x buff;
  let len = List.length !(Buff.get_line buff) in
  match len < cursor_pos.x with false -> () | true -> move_to len cursor_pos.y

(* [arrow_keys dir] shifts the cursor position depending on the direction [dir] which is
   a type of [`Arrow] *)
let arrow_keys direction =
  match !cmd_mode with
  | true -> (
      match direction with
      | `Left when cmd_cusor.x > 0 ->
          cmd_cusor.x <- cmd_cusor.x - 1;
          Buff.left_to_right cmd_buff
      | `Right when cmd_cusor.x < List.length !cmd_line ->
          cmd_cusor.x <- cmd_cusor.x + 1;
          Buff.right_to_left cmd_buff
      | _ -> ())
  | false -> (
      match direction with
      | `Up when cursor_pos.y > 0 ->
          move_by 0 (-1);
          change_line ()
      | `Down when cursor_pos.y < snd dim && cursor_pos.y < Grid.length grid - 1
        ->
          move_by 0 1;
          change_line ()
      | `Right
        when cursor_pos.x < fst dim
             && cursor_pos.x < List.length !(Buff.get_line buff) ->
          move_by 1 0;
          Buff.right_to_left buff
      | `Left when cursor_pos.x > 0 ->
          move_by (-1) 0;
          Buff.left_to_right buff
      | _ -> ())

(* [load_undo] replaces the current grid with the grid stored in the undo list  *)
let load_undo () =
  Grid.from_string grid (Undo.getVal undo);
  let coords = Undo.getPos undo in
  move_to (fst coords) (snd coords);
  change_line ()

(* [special_event ev] is called upon whenever a special event [ev] is triggered,
   e.g. [`Tab], [`Arrow], etc *)
let special_event ev =
  match ev with
  | `Arrow dir -> arrow_keys dir
  | `Tab ->
      Buff.add buff " ";
      Buff.add buff " ";
      Buff.add buff " ";
      Buff.add buff " ";
      check ();
      typing 4
  | (`Backspace | `Delete) when !cmd_mode && List.length !cmd_line > 0 ->
      Buff.delete cmd_buff;
      cmd_cusor.x <- cmd_cusor.x - 1
  | `Backspace | `Delete -> delete ()
  | `Enter ->
      Grid.add_row ~row:cursor_pos.y grid;
      move_to 0 (cursor_pos.y + 1);

      let r_contents = Buff.get_right buff in
      Buff.set buff (Buff.get_left buff) [];
      Buff.ch_buff grid buff cursor_pos.y;
      Buff.set buff [] r_contents
  | _ -> ()

(* simple function to make a list from a string *)
let str_to_lst acc s =
  s |> String.to_seq |> List.of_seq
  |> List.map (String.make 1)
  |> List.append acc

(* [key_presses k m] is a method that processes key presses [k] and the modifier keys [m]
   calling on corresponding methods *)
let key_presses key mods =
  match (key, mods) with
  | x, [] when !cmd_mode ->
      Buff.add cmd_buff (String.make 1 x);
      cmd_cusor.x <- cmd_cusor.x + 1
  | x, [] ->
      Buff.add buff (String.make 1 x);
      check ();
      typing 1
  | 'Z', [ `Ctrl ] ->
      Undo.undo undo;
      load_undo ()
  | 'R', [ `Ctrl ] ->
      Undo.redo undo;
      load_undo ()
  | 'S', [ `Ctrl ] ->
      grid_save Sys.argv.(1) grid;
      cmd_line := str_to_lst [] (get_time ())
  | 'C', [ `Ctrl ] when !cmd_mode = false ->
      cmd_mode := true;
      Buff.set_line cmd_buff cmd_line
  | 'C', [ `Ctrl ] when !cmd_mode ->
      cmd_mode := false;
      cmd_line := [];
      Buff.update_buff cmd_buff
  | _ -> ()

(* simple function to return an image with or without the command line depending on whether
   it is empty or not *)
let get_img () =
  match !cmd_line with
  | [] -> Grid.image grid
  | _ ->
      I.(
        vcrop (1 - snd dim) 0
        @@ string A.empty (List.fold_left (fun acc x -> acc ^ x) "" !cmd_line)
        </> Grid.image grid)

(* simple function to manage variables depending on which modes are active *)
let mode_management term =
  match !cmd_mode with
  | true -> Term.cursor term (Some (cmd_cusor.x, cmd_cusor.y))
  | false -> Term.cursor term (Some (cursor_pos.x, cursor_pos.y))

(* the main loop for the text editor *)
let rec main_loop term =
  Term.image term (get_img ());
  mode_management term;
  match Term.event term with
  | `End | `Key (`Escape, []) -> ()
  | ev ->
      (match ev with
      | `Key ((#Unescape.special as sp), _) -> special_event sp
      | `Key (`ASCII ch, mods) -> key_presses ch mods
      | _ -> ());
      main_loop term

let _ = main_loop term

val save_file : string -> string ->  unit 
(** [save_file name contents] saves the string [contents] to a file named [name] *)

val load_file : string -> string
(** [load_file name] loads the contents of the file named [name] as a string *)

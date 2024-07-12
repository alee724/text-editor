let save_file name contents =
  let channel = Out_channel.open_text name in
  Out_channel.output_string channel contents;
  Out_channel.close channel

let load_file file =
  try In_channel.with_open_text file In_channel.input_all
  with Sys_error _ ->
    save_file file "";
    ""

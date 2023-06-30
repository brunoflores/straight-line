module Parser = Straight_line.Parser_fe
module Interpreter = Straight_line.Interpreter

let () =
  let usage = "Compile" in
  let spec = [] in
  let files = ref [] in
  let readfname s = files := s :: !files in
  let _ = Arg.parse spec readfname usage in
  let file = List.hd !files in
  match Parser.parse file with
  | Ok prog -> Interpreter.interpret prog
  | Error e ->
      Printf.eprintf "%s\n" e;
      exit 1

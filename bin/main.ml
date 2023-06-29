module Ast = Straight_line.Ast
module Parser = Straight_line.Parser_fe

let interpret (prog : Ast.stm list) : unit = failwith ""

let () =
  let usage = "Compile" in
  let spec = [] in
  let files = ref [] in
  let readfname s = files := s :: !files in
  let _ = Arg.parse spec readfname usage in
  let file = List.hd !files in
  match Parser.parse file with
  | Ok prog -> interpret prog
  | Error e ->
      Printf.eprintf "%s\n" e;
      exit 1

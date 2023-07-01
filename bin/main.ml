module Parser = Straight_line.Parser_fe
module Interpreter = Straight_line.Interpreter
module Ast = Straight_line.Ast

let () =
  let usage = "Compile" in
  let spec = [] in
  let files = ref [] in
  let readfname s = files := s :: !files in
  Arg.parse spec readfname usage;
  let file = List.hd !files in
  match Parser.parse file with
  | Ok prog -> (
      try Interpreter.interpret prog
      with Interpreter.Unbound_identifier pos ->
        let header = "Unbound identifier" in
        Printf.eprintf "%s\n%!" header;
        Ast.pretty_print_err pos;
        exit 1)
  | Error (Parser.SyntaxError (header, pos)) ->
      Printf.eprintf "%s\n%!" header;
      Ast.pretty_print_err pos;
      exit 1
  | Error Parser.Empty -> exit 1

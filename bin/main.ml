module Parser = Straight_line.Parser_fe
module Interpreter = Straight_line.Interpreter
module Ast = Straight_line.Ast

let pretty_print_err (p1, p2) =
  let ( { Ast.pos_fname; pos_lnum; pos_cnum = c1; pos_bol = b1 },
        { Ast.pos_cnum = c2; pos_bol = b2; _ } ) =
    (p1, p2)
  in
  let c1 = c1 - b1 in
  let c2 = c2 - b2 in
  let ic = open_in pos_fname in
  let lnum = ref pos_lnum in
  let line = ref "" in
  let caret =
    String.init c2 (fun i -> if i >= c1 && i <= c2 then '^' else ' ')
  in
  try
    while !lnum > 0 do
      line := input_line ic;
      decr lnum
    done;
    let margin = Format.sprintf "%d | " pos_lnum in
    Printf.eprintf "%s%s\n%!" margin !line;
    let margin = String.make (String.length margin) ' ' in
    Printf.eprintf "%s%s\n\n%!" margin caret;
    close_in ic
  with e ->
    close_in_noerr ic;
    raise e

let () =
  let usage = "Compile" in
  let spec = [] in
  let files = ref [] in
  let readfname s = files := s :: !files in
  let _ = Arg.parse spec readfname usage in
  let file = List.hd !files in
  match Parser.parse file with
  | Ok prog -> (
      try Interpreter.interpret prog
      with Interpreter.Unbound_identifier _pos -> ())
  | Error (Parser.SyntaxError (header, pos)) ->
      Printf.eprintf "%s\n%!" header;
      pretty_print_err pos;
      exit 1
  | Error Parser.Empty -> exit 1

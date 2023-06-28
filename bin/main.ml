module Ast = Straight_line.Ast
module Parser = Straight_line.Parser_fe

let () =
  let usage = "Compile" in
  let spec = [] in
  let files = ref [] in
  let readfname s = files := s :: !files in
  let _ = Arg.parse spec readfname usage in
  let _ = Parser.parse !files in
  ()

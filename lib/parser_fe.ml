module ErrorReports = MenhirLib.ErrorReports
module LexerUtil = MenhirLib.LexerUtil
module MenhirInterpreter = Parser.MenhirInterpreter

type err = SyntaxError of string * (Ast.pos * Ast.pos) | Empty

let succeed prog = Ok prog

let fail text buffer checkpoint : (Ast.stm list, err) result =
  let env checkpoint =
    match checkpoint with
    | MenhirInterpreter.HandlingError env -> env
    | _ -> assert false
  in
  let state checkpoint : int =
    match MenhirInterpreter.top (env checkpoint) with
    | Some (MenhirInterpreter.Element (s, _, _, _)) ->
        MenhirInterpreter.number s
    | None -> 0
  in
  let show text positions =
    ErrorReports.extract text positions
    |> ErrorReports.sanitize |> ErrorReports.compress |> ErrorReports.shorten 20
  in
  let get checkpoint i =
    match MenhirInterpreter.get i (env checkpoint) with
    | Some (MenhirInterpreter.Element (_, _, pos1, pos2)) ->
        show text (pos1, pos2)
    | None -> failwith "index out of range (from Menhir): see the source code"
  in
  let last = ErrorReports.last buffer in
  let location = LexerUtil.range last in
  let indication =
    Format.sprintf "Syntax error %s.\n" (ErrorReports.show (show text) buffer)
  in
  let message = ParserMessages.message (state checkpoint) in
  let message = ErrorReports.expand (get checkpoint) message in
  let header = Printf.sprintf "%s%s%s%!" location indication message in
  let l1, l2 =
    match MenhirInterpreter.top (env checkpoint) with
    | Some (MenhirInterpreter.Element (_, _, pos1, pos2)) -> (pos1, pos2)
    | None -> failwith ""
  in
  let l1, l2 = (Ast.pos_of_lex_pos l1, Ast.pos_of_lex_pos l2) in
  Error (SyntaxError (header, (l1, l2)))

let parse (text, lexbuf) : (Ast.stm list, err) result =
  let supplier = MenhirInterpreter.lexer_lexbuf_to_supplier Lexer.read lexbuf in
  let buffer, supplier = ErrorReports.wrap_supplier supplier in
  let checkpoint = Parser.Incremental.prog lexbuf.lex_curr_p in
  let fail = fail text buffer in
  try MenhirInterpreter.loop_handle succeed fail supplier checkpoint
  with Lexer.SyntaxError _pos -> Error Empty

let ( >>= ) = Result.bind

let parse filename : (Ast.stm list, err) result =
  let read filename = Ok (LexerUtil.read filename) in
  read filename >>= parse

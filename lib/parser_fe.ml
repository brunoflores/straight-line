module ErrorReports = MenhirLib.ErrorReports
module LexerUtil = MenhirLib.LexerUtil
module MenhirInterpreter = Parser.MenhirInterpreter

let succeed prog = Ok prog

let fail text buffer checkpoint =
  let env checkpoint =
    match checkpoint with
    | MenhirInterpreter.HandlingError env -> env
    | _ -> assert false
  in
  let state checkpoint : int =
    match MenhirInterpreter.top (env checkpoint) with
    | Some (MenhirInterpreter.Element (s, _, _, _)) ->
        MenhirInterpreter.number s
    | None ->
        (* Hmm... The parser is in its initial state. The incremental API
           currently lacks a way of finding out the number of the initial
           state. It is usually 0, so we return 0. This is unsatisfactory
           and should be fixed in the future. *)
        0
  in
  let show text positions =
    ErrorReports.extract text positions
    |> ErrorReports.sanitize |> ErrorReports.compress |> ErrorReports.shorten 20
  in
  let get checkpoint i =
    match MenhirInterpreter.get i (env checkpoint) with
    | Some (MenhirInterpreter.Element (_, _, pos1, pos2)) ->
        show text (pos1, pos2)
    | None ->
        (* The index is out of range. This should not happen if [$i]
           keywords are correctly inside the syntax error message
           database. The integer [i] should always be a valid offset
           into the known suffix of the stack. *)
        failwith "index out of range (from Menhir): see the source code"
  in
  let location = LexerUtil.range (ErrorReports.last buffer) in
  let indication =
    Format.sprintf "Syntax error %s.\n" (ErrorReports.show (show text) buffer)
  in
  let message = ParserMessages.message (state checkpoint) in
  let message = ErrorReports.expand (get checkpoint) message in
  Error (Printf.sprintf "%s%s%s%!" location indication message)

let parse (lexbuf, text) : (Ast.stm list, string) result =
  let supplier = MenhirInterpreter.lexer_lexbuf_to_supplier Lexer.read lexbuf in
  let buffer, supplier = ErrorReports.wrap_supplier supplier in
  let checkpoint = Parser.Incremental.prog lexbuf.lex_curr_p in
  let fail = fail text buffer in
  try MenhirInterpreter.loop_handle succeed fail supplier checkpoint
  with Lexer.SyntaxError msg -> Error msg

let io_read_text file =
  try
    let text = Stdio.In_channel.read_all file in
    Ok (LexerUtil.init file (text |> Lexing.from_string), text)
  with Sys_error msg -> Error msg

let ( >>= ) = Result.bind
let parse file : (Ast.stm list, string) result = io_read_text file >>= parse

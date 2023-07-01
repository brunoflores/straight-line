type pos = { pos_fname : string; pos_lnum : int; pos_bol : int; pos_cnum : int }
type id = string

type binop = Plus | Minus | Times | Div
and stm = AssignStm of id * exp | PrintStm of exp list

and exp =
  | EffectfulExp of stm * exp
  | IdExp of id * (pos * pos)
  | NumExp of int
  | OpExp of exp * binop * exp

let pos_of_lexing_position (pos : Lexing.position) : pos =
  {
    pos_fname = pos.pos_fname;
    pos_lnum = pos.pos_lnum;
    pos_bol = pos.pos_bol;
    pos_cnum = pos.pos_cnum;
  }

let pretty_print_err (p1, p2) =
  let ( { pos_fname; pos_lnum; pos_cnum = c1; pos_bol = b1 },
        { pos_cnum = c2; pos_bol = b2; _ } ) =
    (p1, p2)
  in
  (* Character offset minus line offset equals column number *)
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
    let reset = "\027[0m" in
    let red = "\027[31m" in
    Printf.eprintf "%s%s%s%s\n%!" red margin caret reset;
    close_in ic
  with e ->
    close_in_noerr ic;
    raise e

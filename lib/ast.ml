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

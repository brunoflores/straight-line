type id = string

type binop = Plus | Minus | Times | Div
and stm = AssignStm of id * exp | PrintStm of exp list

and exp =
  | EffectfulExp of stm * exp
  | IdExp of id
  | NumExp of int
  | OpExp of exp * binop * exp
  | EseqExp of stm * exp

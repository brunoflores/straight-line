type id = string

type binop = Plus | Minus | Times | Div

and stm =
  | CompoundStm of stm * stm
  | AssignStm of id * stm
  | PrintStm of exp list

and exp =
  | IdExp of id
  | NumExp of int
  | OpExp of exp * binop * exp
  | EseqExp of stm * exp

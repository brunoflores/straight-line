type err = SyntaxError of string * (Ast.pos * Ast.pos) | Empty

val parse : string -> (Ast.stm list, err) result

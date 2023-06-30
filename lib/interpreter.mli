exception Unbound_identifier of (Ast.pos * Ast.pos)

val interpret : Ast.stm list -> unit

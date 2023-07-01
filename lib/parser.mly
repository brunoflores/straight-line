%{ open Ast %}

%token <string> ID
%token <int> INT
%token COMMA
%token SEMICOLON
%token ASSIGN
%token PRINT
%token PLUS
%token MINUS
%token TIMES
%token DIV
%token LPAREN
%token RPAREN
%token EOF

%left PLUS MINUS
%left TIMES DIV

%start <stm list> prog

%%

prog:
  | s = sep_stmts*; EOF
   { s }

(* Separated statements must have at least one semicolon in between *)
(* Multiple semicolons in sequence mean nothing *)
sep_stmts:
  | s = stm
  | s = stm; SEMICOLON+
    { s }

stm:
  | id = ID; ASSIGN; e = exp
    { AssignStm (id, e) }
  | PRINT; LPAREN; exps = sep_exps*; RPAREN
    { PrintStm exps }

(* Separated expressions must have one comma in between *)
sep_exps:
  | e = exp
  | e = exp; COMMA
    { e }

exp:
  | LPAREN; e = exp; RPAREN
    { e }
  | LPAREN; s = stm; COMMA; e = exp; RPAREN
    { EffectfulExp (s, e) }
  | id = ID
    { IdExp (id, (pos_of_lexing_position $startpos, pos_of_lexing_position $endpos)) }
  | i = INT
    { NumExp i }
  | e1 = exp; op = binop; e2 = exp
    { OpExp (e1, op, e2) }

%inline binop:
  | PLUS
    { Plus }
  | MINUS
    { Minus }
  | TIMES
    { Times }
  | DIV
    { Div }

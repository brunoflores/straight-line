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
  | stmts = separated_list(SEMICOLON, stm); EOF
    { stmts }

stm:
  | id = ID; ASSIGN; e = exp
    { AssignStm (id, e) }
  | PRINT; LPAREN; exps = separated_nonempty_list(COMMA, exp); RPAREN
    { PrintStm exps }

exp:
  | LPAREN; e = exp; RPAREN
    { e }
  | LPAREN; s = stm; COMMA; e = exp; RPAREN
    { EffectfulExp (s, e) }
  | id = ID
    { IdExp (id, (pos_of_lexing_position $startpos, pos_of_lexing_position $endpos)) }
  | i = INT
    { NumExp i }
  | e1 = exp; PLUS; e2 = exp
    { OpExp (e1, Plus, e2) }
  | e1 = exp; MINUS; e2 = exp
    { OpExp (e1, Minus, e2) }
  | e1 = exp; TIMES; e2 = exp
    { OpExp (e1, Times, e2) }
  | e1 = exp; DIV; e2 = exp
    { OpExp (e1, Div, e2) }

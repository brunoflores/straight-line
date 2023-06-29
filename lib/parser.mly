%{
open Ast
%}

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
  | id = ID; ASSIGN; e = par_exp
    { AssignStm (id, e) }
  | PRINT; LPAREN; exps = separated_nonempty_list(COMMA, par_exp); RPAREN
    { PrintStm exps }

par_exp:
  | LPAREN; e = exp; RPAREN
    { e }
  | e = exp
    { e }

exp:
  | LPAREN; s = stm; COMMA; e = par_exp; RPAREN
    { EffectfulExp (s, e) }
  | id = ID
    { IdExp id }
  | i = INT
    { NumExp i }
  | e1 = par_exp; PLUS; e2 = par_exp
    { OpExp (e1, Plus, e2) }
  | e1 = par_exp; MINUS; e2 = par_exp
    { OpExp (e1, Minus, e2) }
  | e1 = par_exp; TIMES; e2 = par_exp
    { OpExp (e1, Times, e2) }
  | e1 = par_exp; DIV; e2 = par_exp
    { OpExp (e1, Div, e2) }

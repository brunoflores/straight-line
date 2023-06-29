%{
(* open Ast *)
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
%token EOF

%start <unit> prog

%%

prog:
  | EOF { () }

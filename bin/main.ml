module Ast = Straight_line.Ast
module Parser = Straight_line.Parser_fe

type env = (Ast.id * int) list

let rec interpretStm (env : env) (stm : Ast.stm) : env =
  match stm with
  | Ast.AssignStm (id, exp) ->
      let exp, env = interpretExp env exp in
      (* We add this new binding to the environment list trusting that
         [List.assoc] returns the first binding found when searching. *)
      (id, exp) :: env
  | Ast.PrintStm exps ->
      (* [print] takes a list of expressions to print, so we evaluate them from
         left to right, augmenting the environment and passing it to the next *)
      let interpret_and_augment (results, env) e =
        let e', env' = interpretExp env e in
        (e' :: results, env')
      in
      (* As is commonly done, we fold left and then revert *)
      let results, env' = List.fold_left interpret_and_augment ([], env) exps in
      let results = List.rev results in
      let line =
        List.fold_left
          (fun acc num -> Format.sprintf "%s %s" acc (string_of_int num))
          "" results
      in
      print_endline line;
      env'

and interpretExp (env : env) (exp : Ast.exp) : int * env =
  match exp with
  | Ast.NumExp e -> (e, env)
  | Ast.EffectfulExp (s, e) ->
      let env' = interpretStm env s in
      interpretExp env' e
  | Ast.IdExp id -> (
      match List.assoc_opt id env with
      | Some e -> (e, env)
      | None -> failwith @@ Format.sprintf "Unbound identifier: %s\n" id)
  | Ast.OpExp (e1, op, e2) ->
      let e1, env' = interpretExp env e1 in
      let e2, env'' = interpretExp env' e2 in
      let meta_op =
        match op with
        | Ast.Plus -> ( + )
        | Ast.Minus -> ( - )
        | Ast.Times -> ( * )
        | Ast.Div -> ( / )
      in
      let e = meta_op e1 e2 in
      (e, env'')

let interpret (prog : Ast.stm list) : unit =
  let emptyenv : env = [] in
  (* We discard the last environment *)
  let _ = List.fold_left interpretStm emptyenv prog in
  ()

let () =
  let usage = "Compile" in
  let spec = [] in
  let files = ref [] in
  let readfname s = files := s :: !files in
  let _ = Arg.parse spec readfname usage in
  let file = List.hd !files in
  match Parser.parse file with
  | Ok prog -> interpret prog
  | Error e ->
      Printf.eprintf "%s\n" e;
      exit 1

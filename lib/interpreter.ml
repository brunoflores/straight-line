exception Unbound_identifier of (Ast.pos * Ast.pos)

type env = (Ast.id * int) list

let rec interpret_stm (env : env) (stm : Ast.stm) : env =
  match stm with
  | Ast.AssignStm (id, exp) ->
      let exp, env = interpret_exp env exp in
      (* We add this new binding to the environment list trusting that
         [List.assoc] returns the first binding found when searching. *)
      (id, exp) :: env
  | Ast.PrintStm exps ->
      (* Print a line of integers separated by a space char *)
      let print line =
        let _ = List.iter (fun num -> Printf.printf "%d " num) line in
        Printf.printf "\n"
      in
      (* [print] takes a list of expressions to print, so we evaluate them from
         left to right, augmenting the environment and passing it to the next *)
      let interpret_and_augment (results, env) e =
        let e', env' = interpret_exp env e in
        (e' :: results, env')
      in
      (* We fold left and then reverse the resulting list *)
      let results, env' = List.fold_left interpret_and_augment ([], env) exps in
      List.rev results |> print;
      (* We return the environment that resulted from the last expression *)
      env'

and interpret_exp (env : env) (exp : Ast.exp) : int * env =
  match exp with
  | Ast.NumExp e -> (e, env)
  | Ast.EffectfulExp (s, e) ->
      let env' = interpret_stm env s in
      interpret_exp env' e
  | Ast.IdExp (id, pos) -> (
      match List.assoc_opt id env with
      | Some e -> (e, env)
      | None -> raise @@ Unbound_identifier pos)
  | Ast.OpExp (e1, op, e2) ->
      let e1, env' = interpret_exp env e1 in
      let e2, env'' = interpret_exp env' e2 in
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
  let _ = List.fold_left interpret_stm emptyenv prog in
  ()

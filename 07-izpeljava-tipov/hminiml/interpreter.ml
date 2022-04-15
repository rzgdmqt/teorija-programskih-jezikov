module S = Syntax

type 'a result = Res of 'a | Exp of S.exp

let ( >>= ) exp f = match exp with Exp exp -> exp | Res x -> f x

let is_value = function
  | S.Int _ | S.Bool _ | S.Lambda _ | S.RecLambda _ -> true
  | S.Var _ | S.Plus _ | S.Minus _ | S.Times _ | S.Equal _ | S.Less _
  | S.Greater _ | S.IfThenElse _ | S.Apply _ | Raise _ | Try _ ->
      false

let is_terminal = function S.Raise _ -> true | exp -> is_value exp

let rec eval_exp = function
  | S.Var _ -> failwith "Expected a closed term"
  | (S.Int _ | S.Bool _ | S.Lambda _ | S.RecLambda _ | S.Raise _) as e ->
      assert (is_terminal e);
      e
  | S.Plus (e1, e2) ->
      eval_int e1 >>= fun n1 ->
      eval_int e2 >>= fun n2 -> S.Int (n1 + n2)
  | S.Minus (e1, e2) ->
      eval_int e1 >>= fun n1 ->
      eval_int e2 >>= fun n2 -> S.Int (n1 - n2)
  | S.Times (e1, e2) ->
      eval_int e1 >>= fun n1 ->
      eval_int e2 >>= fun n2 -> S.Int (n1 * n2)
  | S.Equal (e1, e2) ->
      eval_int e1 >>= fun n1 ->
      eval_int e2 >>= fun n2 -> S.Bool (n1 = n2)
  | S.Less (e1, e2) ->
      eval_int e1 >>= fun n1 ->
      eval_int e2 >>= fun n2 -> S.Bool (n1 < n2)
  | S.Greater (e1, e2) ->
      eval_int e1 >>= fun n1 ->
      eval_int e2 >>= fun n2 -> S.Bool (n1 > n2)
  | S.IfThenElse (e, e1, e2) -> (
      match eval_exp e with
      | S.Raise _ as exc -> exc
      | S.Bool true -> eval_exp e1
      | S.Bool false -> eval_exp e2
      | _ -> failwith "Boolean expected")
  | S.Apply (e1, e2) -> (
      match eval_exp e1 with
      | S.Raise _ as exc -> exc
      | S.Lambda (x, e) -> (
          match eval_exp e2 with
          | S.Raise _ as exc -> exc
          | v -> eval_exp (S.subst_exp [ (x, v) ] e))
      | S.RecLambda (f, x, e) as rec_f -> (
          match eval_exp e2 with
          | S.Raise _ as exc -> exc
          | v -> eval_exp (S.subst_exp [ (f, rec_f); (x, v) ] e))
      | _ -> failwith "Function expected")
  | S.Try (e1, e2) -> (
      let v = eval_exp e1 in
      match v with S.Raise _ -> eval_exp e2 | _ -> v)

and eval_int (e : S.exp) : int result =
  match eval_exp e with
  | S.Int n -> Res n
  | S.Raise _ as exc -> Exp exc
  | _ -> failwith "Integer expected"

let rec step e =
  print_endline (S.string_of_exp e);
  match e with
  | S.Var _ | S.Int _ | S.Bool _ | S.Lambda _ | S.RecLambda _ | S.Raise _ ->
      failwith "Expected a non-terminal expression"
  | S.Plus (S.Raise e, _) | S.Plus (S.Int _, S.Raise e) -> S.Raise e
  | S.Plus (S.Int n1, S.Int n2) -> S.Int (n1 + n2)
  | S.Plus (S.Int n1, e2) -> S.Plus (S.Int n1, step e2)
  | S.Plus (e1, e2) -> S.Plus (step e1, e2)
  | S.Minus (S.Raise e, _) | S.Minus (S.Int _, S.Raise e) -> S.Raise e
  | S.Minus (S.Int n1, S.Int n2) -> S.Int (n1 - n2)
  | S.Minus (S.Int n1, e2) -> S.Minus (S.Int n1, step e2)
  | S.Minus (e1, e2) -> S.Minus (step e1, e2)
  | S.Times (S.Raise e, _) | S.Times (S.Int _, S.Raise e) -> S.Raise e
  | S.Times (S.Int n1, S.Int n2) -> S.Int (n1 * n2)
  | S.Times (S.Int n1, e2) -> S.Times (S.Int n1, step e2)
  | S.Times (e1, e2) -> S.Times (step e1, e2)
  | S.Equal ((S.Raise _ as exc), _) | S.Equal (S.Int _, (S.Raise _ as exc)) ->
      exc
  | S.Equal (S.Int n1, S.Int n2) -> S.Bool (n1 = n2)
  | S.Equal (S.Int n1, e2) -> S.Equal (S.Int n1, step e2)
  | S.Equal (e1, e2) -> S.Equal (step e1, e2)
  | S.Less ((S.Raise _ as exc), _) | S.Less (S.Int _, (S.Raise _ as exc)) -> exc
  | S.Less (S.Int n1, S.Int n2) -> S.Bool (n1 < n2)
  | S.Less (S.Int n1, e2) -> S.Less (S.Int n1, step e2)
  | S.Less (e1, e2) -> S.Less (step e1, e2)
  | S.Greater ((S.Raise _ as exc), _) | S.Greater (S.Int _, (S.Raise _ as exc))
    ->
      exc
  | S.Greater (S.Int n1, S.Int n2) -> S.Bool (n1 > n2)
  | S.Greater (S.Int n1, e2) -> S.Greater (S.Int n1, step e2)
  | S.Greater (e1, e2) -> S.Greater (step e1, e2)
  | S.IfThenElse (S.Raise e, _, _) -> S.Raise e
  | S.IfThenElse (S.Bool b, e1, e2) -> if b then e1 else e2
  | S.IfThenElse (e, e1, e2) -> S.IfThenElse (step e, e1, e2)
  | S.Apply ((S.Raise _ as exc), _) -> exc
  | S.Apply (v, (S.Raise _ as exc)) when is_value v -> exc
  | S.Apply (S.Lambda (x, e), v) when is_value v -> S.subst_exp [ (x, v) ] e
  | S.Apply ((S.RecLambda (f, x, e) as rec_f), v) when is_value v ->
      S.subst_exp [ (f, rec_f); (x, v) ] e
  | S.Apply (((S.Lambda _ | S.RecLambda _) as f), e) -> S.Apply (f, step e)
  | S.Apply (e1, e2) -> S.Apply (step e1, e2)
  | S.Try (S.Raise _, e2) -> e2
  | S.Try (v, _) when is_value v -> v
  | S.Try (e1, e2) -> S.Try (step e1, e2)

let big_step e =
  let v = eval_exp e in
  print_endline (S.string_of_exp v)

let rec small_step e =
  print_endline (S.string_of_exp e);
  if not (is_terminal e) then (
    print_endline "  ~>";
    small_step (step e))

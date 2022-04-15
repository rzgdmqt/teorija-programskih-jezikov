module S = Syntax

module IdentMap = Map.Make (struct
  type t = S.ident

  let compare (S.Ident x) (S.Ident y) = compare x y
end)

type 'a env = 'a IdentMap.t

type state = int env

let print_state state =
  IdentMap.iter
    (fun id v ->
      print_endline (S.string_of_ident id ^ " = " ^ string_of_int v ^ "; "))
    state

type result = state * S.exp

type 'a temp_result = Res of 'a | Exp of result

let ( >>= ) exp f = match exp with Exp exp -> exp | Res x -> f x

let is_value = function
  | S.Int _ | S.Bool _ | S.Lambda _ | S.RecLambda _ | S.Unit -> true
  | S.Var _ | S.Plus _ | S.Minus _ | S.Times _ | S.Equal _ | S.Less _
  | S.Greater _ | S.IfThenElse _ | S.Apply _ | Raise _ | Try _ | S.Read _
  | S.Assign _ ->
      false

let is_terminal = function S.Raise _ -> true | exp -> is_value exp

let rec eval_exp (state : state) (exp : S.exp) : result =
  match exp with
  | S.Var _ -> failwith "Expected a closed term"
  | (S.Int _ | S.Bool _ | S.Lambda _ | S.RecLambda _ | S.Raise _ | S.Unit) as e
    ->
      assert (is_terminal e);
      (state, e)
  | S.Plus (e1, e2) ->
      eval_int state e1 >>= fun (state, n1) ->
      eval_int state e2 >>= fun (state, n2) -> (state, S.Int (n1 + n2))
  | S.Minus (e1, e2) ->
      eval_int state e1 >>= fun (state, n1) ->
      eval_int state e2 >>= fun (state, n2) -> (state, S.Int (n1 - n2))
  | S.Times (e1, e2) ->
      eval_int state e1 >>= fun (state, n1) ->
      eval_int state e2 >>= fun (state, n2) -> (state, S.Int (n1 * n2))
  | S.Equal (e1, e2) ->
      eval_int state e1 >>= fun (state, n1) ->
      eval_int state e2 >>= fun (state, n2) -> (state, S.Bool (n1 = n2))
  | S.Less (e1, e2) ->
      eval_int state e1 >>= fun (state, n1) ->
      eval_int state e2 >>= fun (state, n2) -> (state, S.Bool (n1 < n2))
  | S.Greater (e1, e2) ->
      eval_int state e1 >>= fun (state, n1) ->
      eval_int state e2 >>= fun (state, n2) -> (state, S.Bool (n1 > n2))
  | S.IfThenElse (e, e1, e2) -> (
      match eval_exp state e with
      | state, (S.Raise _ as exc) -> (state, exc)
      | state, S.Bool true -> eval_exp state e1
      | state, S.Bool false -> eval_exp state e2
      | _ -> failwith "Boolean expected")
  | S.Apply (e1, e2) -> (
      match eval_exp state e1 with
      | state, (S.Raise _ as exc) -> (state, exc)
      | state, S.Lambda (x, e) -> (
          match eval_exp state e2 with
          | state, (S.Raise _ as exc) -> (state, exc)
          | state, v -> eval_exp state (S.subst_exp [ (x, v) ] e))
      | state, (S.RecLambda (f, x, e) as rec_f) -> (
          match eval_exp state e2 with
          | state, (S.Raise _ as exc) -> (state, exc)
          | state, v -> eval_exp state (S.subst_exp [ (f, rec_f); (x, v) ] e))
      | _ -> failwith "Function expected")
  | S.Try (e1, e2) -> (
      let v = eval_exp state e1 in
      match v with state, S.Raise _ -> eval_exp state e2 | _ -> v)
  | S.Assign (x, e) ->
      eval_int state e >>= fun (state, v) ->
      let state = IdentMap.add x v state in
      (state, S.Unit)
  | S.Read x -> (
      match IdentMap.find_opt x state with
      | Some n -> (state, S.Int n)
      | None -> (state, S.Raise (S.Ident "Not found")))

and eval_int (state : state) (e : S.exp) =
  match eval_exp state e with
  | state, S.Int n -> Res (state, n)
  | state, S.Read l -> (
      match IdentMap.find_opt l state with
      | Some n -> Res (state, n)
      | None -> Exp (state, S.Raise (S.Ident "Not found")))
  | state, (S.Raise _ as exc) -> Exp (state, exc)
  | _ -> failwith "Integer expected"

let rec step (state : state) e =
  print_endline (S.string_of_exp e);
  match e with
  | S.Var _ | S.Int _ | S.Bool _ | S.Lambda _ | S.RecLambda _ | S.Raise _
  | S.Unit ->
      failwith "Expected a non-terminal expression"
  | S.Plus (S.Raise e, _) | S.Plus (S.Int _, S.Raise e) -> (state, S.Raise e)
  | S.Plus (S.Int n1, S.Int n2) -> (state, S.Int (n1 + n2))
  | S.Plus (S.Int n1, e2) ->
      let state, e2 = step state e2 in
      (state, S.Plus (S.Int n1, e2))
  | S.Plus (e1, e2) ->
      let state, e1 = step state e1 in
      (state, S.Plus (e1, e2))
  | S.Minus (S.Raise e, _) | S.Minus (S.Int _, S.Raise e) -> (state, S.Raise e)
  | S.Minus (S.Int n1, S.Int n2) -> (state, S.Int (n1 - n2))
  | S.Minus (S.Int n1, e2) ->
      let state, e2 = step state e2 in
      (state, S.Minus (S.Int n1, e2))
  | S.Minus (e1, e2) ->
      let state, e1 = step state e1 in
      (state, S.Minus (e1, e2))
  | S.Times (S.Raise e, _) | S.Times (S.Int _, S.Raise e) -> (state, S.Raise e)
  | S.Times (S.Int n1, S.Int n2) -> (state, S.Int (n1 * n2))
  | S.Times (S.Int n1, e2) ->
      let state, e2 = step state e2 in
      (state, S.Times (S.Int n1, e2))
  | S.Times (e1, e2) ->
      let state, e1 = step state e1 in
      (state, S.Times (e1, e2))
  | S.Equal ((S.Raise _ as exc), _) | S.Equal (S.Int _, (S.Raise _ as exc)) ->
      (state, exc)
  | S.Equal (S.Int n1, S.Int n2) -> (state, S.Bool (n1 = n2))
  | S.Equal (S.Int n1, e2) ->
      let state, e2 = step state e2 in
      (state, S.Equal (S.Int n1, e2))
  | S.Equal (e1, e2) ->
      let state, e1 = step state e1 in
      (state, S.Equal (e1, e2))
  | S.Less ((S.Raise _ as exc), _) | S.Less (S.Int _, (S.Raise _ as exc)) ->
      (state, exc)
  | S.Less (S.Int n1, S.Int n2) -> (state, S.Bool (n1 < n2))
  | S.Less (S.Int n1, e2) ->
      let state, e2 = step state e2 in
      (state, S.Less (S.Int n1, e2))
  | S.Less (e1, e2) ->
      let state, e1 = step state e1 in
      (state, S.Less (e1, e2))
  | S.Greater ((S.Raise _ as exc), _) | S.Greater (S.Int _, (S.Raise _ as exc))
    ->
      (state, exc)
  | S.Greater (S.Int n1, S.Int n2) -> (state, S.Bool (n1 > n2))
  | S.Greater (S.Int n1, e2) ->
      let state, e2 = step state e2 in
      (state, S.Greater (S.Int n1, e2))
  | S.Greater (e1, e2) ->
      let state, e1 = step state e1 in
      (state, S.Greater (e1, e2))
  | S.IfThenElse (S.Raise e, _, _) -> (state, S.Raise e)
  | S.IfThenElse (S.Bool b, e1, e2) -> (state, if b then e1 else e2)
  | S.IfThenElse (e, e1, e2) ->
      let state, e = step state e in
      (state, S.IfThenElse (e, e1, e2))
  | S.Apply ((S.Raise _ as exc), _) -> (state, exc)
  | S.Apply (v, (S.Raise _ as exc)) when is_value v -> (state, exc)
  | S.Apply (S.Lambda (x, e), v) when is_value v ->
      (state, S.subst_exp [ (x, v) ] e)
  | S.Apply ((S.RecLambda (f, x, e) as rec_f), v) when is_value v ->
      (state, S.subst_exp [ (f, rec_f); (x, v) ] e)
  | S.Apply (((S.Lambda _ | S.RecLambda _) as f), e) ->
      let state, e = step state e in
      (state, S.Apply (f, e))
  | S.Apply (e1, e2) ->
      let state, e1 = step state e1 in
      (state, S.Apply (e1, e2))
  | S.Try (S.Raise _, e2) -> (state, e2)
  | S.Try (v, _) when is_value v -> (state, v)
  | S.Try (e1, e2) ->
      let state, e1 = step state e1 in
      (state, S.Try (e1, e2))
  | S.Assign (l, S.Int v) -> (IdentMap.add l v state, S.Unit)
  | S.Assign (l, e) ->
      let state, e = step state e in
      (state, S.Assign (l, e))
  | S.Read l -> (
      ( state,
        match IdentMap.find_opt l state with
        | Some v -> S.Int v
        | None -> S.Raise (S.Ident "Not found") ))

let empty_state = IdentMap.empty

let big_step state e =
  let state, v = eval_exp state e in
  print_state state;
  print_endline (S.string_of_exp v)

let rec small_step state e =
  print_state state;
  print_newline ();
  print_endline (S.string_of_exp e);
  if not (is_terminal e) then (
    print_endline "  ~>";
    let state, e = step state e in
    small_step state e)

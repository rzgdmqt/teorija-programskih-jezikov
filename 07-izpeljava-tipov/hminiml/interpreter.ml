module S = Syntax

module IdentMap = Map.Make (struct
  type t = S.ident

  let compare (S.Ident x) (S.Ident y) = compare x y
end)

type 'a env = 'a IdentMap.t

type state = int env

type printed = int list

let print_state state =
  IdentMap.iter
    (fun id v ->
      print_endline (S.string_of_ident id ^ " = " ^ string_of_int v ^ "; "))
    state

type result = state * S.exp * printed

let ( >> ) (state, exp, printed) f =
  let state, exp', printed' = f (state, exp) in
  (state, exp', printed @ printed')

let ( >-> ) (state, exp, printed) f =
  let state, exp' = f (state, exp) in
  (state, exp', printed)

let empty_print (state, exp) = (state, exp, [])

type 'a temp_result = Res of 'a | Exp of result

let ( >>= ) exp f = match exp with Exp exp -> exp | Res x -> x >> f

let is_value = function
  | S.Int _ | S.Bool _ | S.Lambda _ | S.RecLambda _ | S.Unit -> true
  | S.Var _ | S.Plus _ | S.Minus _ | S.Times _ | S.Equal _ | S.Less _
  | S.Greater _ | S.IfThenElse _ | S.Apply _ | Raise _ | Try _ | S.Read _
  | S.Assign _ | S.Print _ ->
      false

let is_terminal = function S.Raise _ -> true | exp -> is_value exp

let rec eval_exp (state : state) (exp : S.exp) : result =
  match exp with
  | S.Var _ -> failwith "Expected a closed term"
  | (S.Int _ | S.Bool _ | S.Lambda _ | S.RecLambda _ | S.Raise _ | S.Unit) as e
    ->
      assert (is_terminal e);
      empty_print (state, e)
  | S.Plus (e1, e2) ->
      eval_int state e1 >>= fun (state, n1) ->
      eval_int state e2 >>= fun (state, n2) ->
      empty_print (state, S.Int (n1 + n2))
  | S.Minus (e1, e2) ->
      eval_int state e1 >>= fun (state, n1) ->
      eval_int state e2 >>= fun (state, n2) ->
      empty_print (state, S.Int (n1 - n2))
  | S.Times (e1, e2) ->
      eval_int state e1 >>= fun (state, n1) ->
      eval_int state e2 >>= fun (state, n2) ->
      empty_print (state, S.Int (n1 * n2))
  | S.Equal (e1, e2) ->
      eval_int state e1 >>= fun (state, n1) ->
      eval_int state e2 >>= fun (state, n2) ->
      empty_print (state, S.Bool (n1 = n2))
  | S.Less (e1, e2) ->
      eval_int state e1 >>= fun (state, n1) ->
      eval_int state e2 >>= fun (state, n2) ->
      empty_print (state, S.Bool (n1 < n2))
  | S.Greater (e1, e2) ->
      eval_int state e1 >>= fun (state, n1) ->
      eval_int state e2 >>= fun (state, n2) ->
      empty_print (state, S.Bool (n1 > n2))
  | S.IfThenElse (e, e1, e2) -> (
      eval_exp state e >> function
      | state, (S.Raise _ as exc) -> empty_print (state, exc)
      | state, S.Bool true -> eval_exp state e1
      | state, S.Bool false -> eval_exp state e2
      | _ -> failwith "Boolean expected")
  | S.Apply (e1, e2) -> (
      eval_exp state e1 >> function
      | state, (S.Raise _ as exc) -> empty_print (state, exc)
      | state, S.Lambda (x, e) -> (
          eval_exp state e2 >> function
          | state, (S.Raise _ as exc) -> empty_print (state, exc)
          | state, v -> eval_exp state (S.subst_exp [ (x, v) ] e))
      | state, (S.RecLambda (f, x, e) as rec_f) -> (
          eval_exp state e2 >> function
          | state, (S.Raise _ as exc) -> empty_print (state, exc)
          | state, v -> eval_exp state (S.subst_exp [ (f, rec_f); (x, v) ] e))
      | _ -> failwith "Function expected")
  | S.Try (e1, e2) -> (
      eval_exp state e1 >> function
      | state, S.Raise _ -> eval_exp state e2
      | v -> empty_print v)
  | S.Assign (x, e) ->
      eval_int state e >>= fun (state, v) ->
      let state = IdentMap.add x v state in
      empty_print (state, S.Unit)
  | S.Read x -> (
      match IdentMap.find_opt x state with
      | Some n -> empty_print (state, S.Int n)
      | None -> empty_print (state, S.Raise (S.Ident "Not found")))
  | Print e -> (
      eval_exp state e >> function
      | state, (S.Raise _ as exc) -> empty_print (state, exc)
      | state, S.Int v -> (state, S.Unit, [ v ])
      | _ -> failwith "Integer expected")

and eval_int (state : state) (e : S.exp) : (state * int * printed) temp_result =
  match eval_exp state e with
  | state, S.Int n, o -> Res (state, n, o)
  | state, S.Read l, o -> (
      match IdentMap.find_opt l state with
      | Some n -> Res (state, n, o)
      | None -> Exp (state, S.Raise (S.Ident "Not found"), o))
  | state, (S.Raise _ as exc), o -> Exp (state, exc, o)
  | _ -> failwith "Integer expected"

let rec step (state : state) e =
  match e with
  | S.Var _ | S.Int _ | S.Bool _ | S.Lambda _ | S.RecLambda _ | S.Raise _
  | S.Unit ->
      failwith "Expected a non-terminal expression"
  | S.Plus (S.Raise e, _) | S.Plus (S.Int _, S.Raise e) ->
      empty_print (state, S.Raise e)
  | S.Plus (S.Int n1, S.Int n2) -> empty_print (state, S.Int (n1 + n2))
  | S.Plus (S.Int n1, e2) ->
      step state e2 >-> fun (state, e2) -> (state, S.Plus (S.Int n1, e2))
  | S.Plus (e1, e2) ->
      step state e1 >-> fun (state, e1) -> (state, S.Plus (e1, e2))
  | S.Minus (S.Raise e, _) | S.Minus (S.Int _, S.Raise e) ->
      empty_print (state, S.Raise e)
  | S.Minus (S.Int n1, S.Int n2) -> empty_print (state, S.Int (n1 - n2))
  | S.Minus (S.Int n1, e2) ->
      step state e2 >-> fun (state, e2) -> (state, S.Minus (S.Int n1, e2))
  | S.Minus (e1, e2) ->
      step state e1 >-> fun (state, e1) -> (state, S.Minus (e1, e2))
  | S.Times (S.Raise e, _) | S.Times (S.Int _, S.Raise e) ->
      empty_print (state, S.Raise e)
  | S.Times (S.Int n1, S.Int n2) -> empty_print (state, S.Int (n1 * n2))
  | S.Times (S.Int n1, e2) ->
      step state e2 >-> fun (state, e2) -> (state, S.Times (S.Int n1, e2))
  | S.Times (e1, e2) ->
      step state e1 >-> fun (state, e1) -> (state, S.Times (e1, e2))
  | S.Equal ((S.Raise _ as exc), _) | S.Equal (S.Int _, (S.Raise _ as exc)) ->
      empty_print (state, exc)
  | S.Equal (S.Int n1, S.Int n2) -> empty_print (state, S.Bool (n1 = n2))
  | S.Equal (S.Int n1, e2) ->
      step state e2 >-> fun (state, e2) -> (state, S.Equal (S.Int n1, e2))
  | S.Equal (e1, e2) ->
      step state e1 >-> fun (state, e1) -> (state, S.Equal (e1, e2))
  | S.Less ((S.Raise _ as exc), _) | S.Less (S.Int _, (S.Raise _ as exc)) ->
      empty_print (state, exc)
  | S.Less (S.Int n1, S.Int n2) -> empty_print (state, S.Bool (n1 < n2))
  | S.Less (S.Int n1, e2) ->
      step state e2 >-> fun (state, e2) -> (state, S.Less (S.Int n1, e2))
  | S.Less (e1, e2) ->
      step state e1 >-> fun (state, e1) -> (state, S.Less (e1, e2))
  | S.Greater ((S.Raise _ as exc), _) | S.Greater (S.Int _, (S.Raise _ as exc))
    ->
      empty_print (state, exc)
  | S.Greater (S.Int n1, S.Int n2) -> empty_print (state, S.Bool (n1 > n2))
  | S.Greater (S.Int n1, e2) ->
      step state e2 >-> fun (state, e2) -> (state, S.Greater (S.Int n1, e2))
  | S.Greater (e1, e2) ->
      step state e1 >-> fun (state, e1) -> (state, S.Greater (e1, e2))
  | S.IfThenElse (S.Raise e, _, _) -> empty_print (state, S.Raise e)
  | S.IfThenElse (S.Bool b, e1, e2) -> empty_print (state, if b then e1 else e2)
  | S.IfThenElse (e, e1, e2) ->
      step state e >-> fun (state, e) -> (state, S.IfThenElse (e, e1, e2))
  | S.Apply ((S.Raise _ as exc), _) -> empty_print (state, exc)
  | S.Apply (v, (S.Raise _ as exc)) when is_value v -> empty_print (state, exc)
  | S.Apply (S.Lambda (x, e), v) when is_value v ->
      empty_print (state, S.subst_exp [ (x, v) ] e)
  | S.Apply ((S.RecLambda (f, x, e) as rec_f), v) when is_value v ->
      empty_print (state, S.subst_exp [ (f, rec_f); (x, v) ] e)
  | S.Apply (((S.Lambda _ | S.RecLambda _) as f), e) ->
      step state e >-> fun (state, e) -> (state, S.Apply (f, e))
  | S.Apply (e1, e2) ->
      step state e1 >-> fun (state, e1) -> (state, S.Apply (e1, e2))
  | S.Try (S.Raise _, e2) -> empty_print (state, e2)
  | S.Try (v, _) when is_value v -> empty_print (state, v)
  | S.Try (e1, e2) ->
      step state e1 >-> fun (state, e1) -> (state, S.Try (e1, e2))
  | S.Assign (l, S.Int v) -> empty_print (IdentMap.add l v state, S.Unit)
  | S.Assign (l, e) ->
      step state e >-> fun (state, e) -> (state, S.Assign (l, e))
  | S.Read l ->
      empty_print
        ( state,
          match IdentMap.find_opt l state with
          | Some v -> S.Int v
          | None -> S.Raise (S.Ident "Not found") )
  | S.Print (S.Int x) -> (state, S.Unit, [ x ])
  | S.Print e -> step state e >-> fun (state, e) -> (state, S.Print e)

let empty_state = IdentMap.empty

let big_step state e =
  let state, v, printed = eval_exp state e in
  print_endline "PRINTED:";
  List.iter
    (fun v ->
      print_int v;
      print_string ", ")
    printed;
  print_newline ();
  print_state state;
  print_endline (S.string_of_exp v)

let rec small_step state e =
  print_state state;
  print_newline ();
  print_endline (S.string_of_exp e);
  if not (is_terminal e) then (
    print_endline "  ~>";
    let state, e, printed = step state e in
    (match printed with
    | [] -> ()
    | _ ->
        print_endline "PRINTED:";
        List.iter
          (fun v ->
            print_int v;
            print_string ", ")
          printed;
        print_newline ());
    small_step state e)

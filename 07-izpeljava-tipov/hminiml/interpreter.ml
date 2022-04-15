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

type result = state * S.cmp * printed

let ( >> ) (state, exp, printed) f =
  let state, exp', printed' = f (state, exp) in
  (state, exp', printed @ printed')

let ( >-> ) (state, exp, printed) f =
  let state, exp' = f (state, exp) in
  (state, exp', printed)

let empty_print (state, exp) = (state, exp, [])

type 'a temp_result = Res of 'a | Exp of result

let ( >>= ) exp f = match exp with Exp exp -> exp | Res x -> x >> f

let is_terminal = function S.Raise _ | S.Return _ -> true | _ -> false

let eval_int_v = function S.Int x -> x | _ -> failwith "expected int"

let rec eval_cmp (state : state) (exp : S.cmp) : result =
  let st v = empty_print (state, S.Return v) in
  match exp with
  | S.Return v -> st v
  | S.Raise _ as exc -> empty_print (state, exc)
  | S.Plus (e1, e2) -> st (S.Int (eval_int_v e1 + eval_int_v e2))
  | S.Minus (e1, e2) -> st (S.Int (eval_int_v e1 - eval_int_v e2))
  | S.Times (e1, e2) -> st (S.Int (eval_int_v e1 * eval_int_v e2))
  | S.Equal (e1, e2) -> st (S.Bool (eval_int_v e1 = eval_int_v e2))
  | S.Less (e1, e2) -> st (S.Bool (eval_int_v e1 < eval_int_v e2))
  | S.Greater (e1, e2) -> st (S.Bool (eval_int_v e1 > eval_int_v e2))
  | S.IfThenElse (v, e1, e2) -> (
      match v with
      | S.Bool b -> if b then eval_cmp state e1 else eval_cmp state e2
      | _ -> failwith "expected bool")
  | S.Apply (v1, v2) -> (
      match v1 with
      | S.Lambda (x, e1) -> eval_cmp state (S.subst_cmp [ (x, v2) ] e1)
      | S.RecLambda (f, (x, e)) as rec_f ->
          eval_cmp state (S.subst_cmp [ (f, rec_f); (x, v2) ] e)
      | _ -> failwith "expected lambda")
  | S.Let (m, (x, n)) -> (
      eval_cmp state m >> function
      | state, (S.Raise _ as exc) -> (state, exc, [])
      | state, S.Return v -> eval_cmp state (S.subst_cmp [ (x, v) ] n)
      | _ -> failwith "Not safe")
  | S.Read v -> (
      match IdentMap.find_opt v state with
      | Some n -> empty_print (state, S.Return (S.Int n))
      | None -> empty_print (state, S.Raise (S.Ident "Not found")))
  | S.Print v -> (state, S.Return S.Unit, [ eval_int_v v ])
  | S.Assign (l, v) ->
      empty_print (IdentMap.add l (eval_int_v v) state, S.Return S.Unit)
  | Try (c1, c2) -> (
      eval_cmp state c1 >> function
      | state, S.Raise _ -> eval_cmp state c2
      | state, S.Return v -> empty_print (state, S.Return v)
      | _ -> failwith "Not safe")

let rec step (state : state) e =
  match e with
  | S.Return _ | S.Raise _ -> failwith "Expected a non-terminal expression"
  | S.Plus (e1, e2) ->
      empty_print (state, S.Return (S.Int (eval_int_v e1 + eval_int_v e2)))
  | S.Minus (e1, e2) ->
      empty_print (state, S.Return (S.Int (eval_int_v e1 - eval_int_v e2)))
  | S.Times (e1, e2) ->
      empty_print (state, S.Return (S.Int (eval_int_v e1 * eval_int_v e2)))
  | S.Equal (e1, e2) ->
      empty_print (state, S.Return (S.Bool (eval_int_v e1 = eval_int_v e2)))
  | S.Less (e1, e2) ->
      empty_print (state, S.Return (S.Bool (eval_int_v e1 < eval_int_v e2)))
  | S.Greater (e1, e2) ->
      empty_print (state, S.Return (S.Bool (eval_int_v e1 > eval_int_v e2)))
  | S.IfThenElse (v, e1, e2) -> (
      match v with
      | S.Bool b -> empty_print (state, if b then e1 else e2)
      | _ -> failwith "expected bool")
  | S.Apply (v1, v2) -> (
      match v1 with
      | S.Lambda (x, e1) -> empty_print (state, S.subst_cmp [ (x, v2) ] e1)
      | S.RecLambda (f, (x, e)) as rec_f ->
          empty_print (state, S.subst_cmp [ (f, rec_f); (x, v2) ] e)
      | _ -> failwith "expected lambda")
  | Let ((Raise _ as exc), _) -> empty_print (state, exc)
  | Let (Return v, (x, c2)) -> empty_print (state, S.subst_cmp [ (x, v) ] c2)
  | Let (c1, c2) -> (
      step state c1 >-> function state, c1 -> (state, S.Let (c1, c2)))
  | Try (Raise _, c2) -> empty_print (state, c2)
  | Try (Return v, _) -> empty_print (state, S.Return v)
  | Try (c1, c2) -> (
      step state c1 >-> function state, c1 -> (state, S.Try (c1, c2)))
  | Assign (l, v) ->
      empty_print (IdentMap.add l (eval_int_v v) state, S.Return S.Unit)
  | Read l -> empty_print (state, S.Return (S.Int (IdentMap.find l state)))
  | Print v -> (state, S.Return S.Unit, [ eval_int_v v ])

let empty_state = IdentMap.empty

let big_step state e =
  let state, v, printed = eval_cmp state e in
  print_endline "PRINTED:";
  List.iter
    (fun v ->
      print_int v;
      print_string ", ")
    printed;
  print_newline ();
  print_state state;
  print_endline (S.string_of_cmp v)

let rec small_step state e =
  print_state state;
  print_newline ();
  print_endline (S.string_of_cmp e);
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

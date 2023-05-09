open Luaparser.Ast

type value = Value.t
type coroutine = Value.coroutine
type env = Value.env

open Value

let map_cps map list (k : 'a list -> unit) =
  let res = ref [] in
  let rec aux = function
    | [] -> ()
    | x :: xs ->
        map x (fun v ->
            res := v :: !res;
            aux xs)
  in
  aux list;
  k (List.rev !res)

let iter_cps iter list (k : unit -> unit) =
  let rec aux = function
    | [] -> k ()
    | el :: elements -> iter el (fun () -> aux elements)
  in
  aux list

let create_scope (names : string list) (values : value list) :
    (name, value) Hashtbl.t =
  let scope = Hashtbl.create (List.length names) in
  let rec aux names values =
    match (names, values) with
    | [], _ -> ()
    | n :: ns, v :: vs ->
        Hashtbl.add scope n v;
        aux ns vs
    | n :: ns, [] ->
        Hashtbl.add scope n Nil;
        aux ns []
  in
  aux names values;
  scope

(* Fonctions de l'interprète, mutuellement récursives. Une fonction par
   catégorie syntaxique de l'AST. *)

(* Interprète un bloc de code *)

let rec interp_block (env : env) (blk : block) (k : value -> unit)
    (co : coroutine) : unit =
  let env = { env with locals = create_scope blk.locals [] :: env.locals } in
  interp_stat env blk.body (fun () -> interp_exp env blk.ret k)

(* Interprète une liste de statements *)

(* Interprète un statement *)

and interp_stat (env : env) (stat : stat) (k : unit -> unit) (co : coroutine) :
    unit =
  match stat with
  | Nop -> k ()
  | Seq (s1, s2) -> interp_stat env s1 (fun () -> interp_stat env s2 k)
  | Assign (v, e) -> (
      match v with
      | Name n ->
          interp_exp env e (fun v ->
              Value.set_ident env n v;
              k ())
      | IndexTable (tab, key) ->
          interp_exp env tab (fun t ->
              interp_exp env key (fun key ->
                  interp_exp env e (fun v ->
                      Hashtbl.replace (as_table t) (as_table_key key) v;
                      k ()))))
  | FunctionCall fc -> interp_funcall env fc (fun _ -> k ())
  | WhileDoEnd (e, s) ->
      let rec cond () =
        interp_exp env e (fun b ->
            if as_bool b then interp_stat env s cond else k ())
      in
      cond ()
  | If (e, s1, s2) ->
      interp_exp env e (fun b ->
          if as_bool b then interp_stat env s1 k else interp_stat env s2 k)

(* Interprète un appel de fonction *)
and interp_funcall (env : env) (fc : functioncall) (k : value -> unit)
    (co : coroutine) : unit =
  let open Value in
  let f, (args : exp list) = fc in
  (* let args = List.map (interp_exp env) args in *)
  (* cps version of map: *)
  map_cps (interp_exp env) args (fun args ->
      interp_exp env f (fun f ->
          match Value.as_function f with
          | Print ->
              List.map Value.to_string args
              |> String.concat "\t" |> print_endline;
              k Nil
          | Closure (args_name, env, block) ->
              (* check if nb arguments match *)
              let env =
                { env with locals = create_scope args_name args :: env.locals }
              in
              interp_block env block k
          | CoroutResume -> assert false
          | CoroutStatus -> assert false
          | CoroutYield -> assert false
          | CoroutCreate -> assert false))
(* Interprète une liste d'expressions *)

(* Interprète une expression *)
and interp_exp (env : env) (e : exp) (k : value -> unit) (co : coroutine) : unit
    =
  match e with
  | Nil -> Nil |> k
  | False -> Bool false |> k
  | True -> Bool true |> k
  | Integer i -> Int i |> k
  | Float f -> Float f |> k
  | LiteralString s -> String s |> k
  | Table key_values ->
      let table = Hashtbl.create (List.length key_values) in
      iter_cps
        (fun (key, v) k ->
          interp_exp env key (fun key ->
              interp_exp env v (fun v ->
                  Hashtbl.add table (as_table_key key) v;
                  k ())))
        key_values
        (fun () -> k (Table table))
  | UnOp (UnaryMinus, e) -> interp_exp env e (fun v -> k @@ neg v)
  | UnOp (Not, e) ->
      (* if as_bool @@ interp_exp env e then Bool false else Bool true *)
      interp_exp env e (fun v ->
          k (if as_bool v then Bool false else Bool true))
  | BinOp (LogicalAnd, e1, e2) ->
      interp_exp env e1 (fun v1 ->
          if as_bool v1 then interp_exp env e2 k else k v1)
  | BinOp (LogicalOr, e1, e2) ->
      interp_exp env e1 (fun v1 ->
          if as_bool v1 then k v1 else interp_exp env e2 k)
  | BinOp (op, e1, e2) ->
      interp_exp env e1 (fun v1 ->
          interp_exp env e2 (fun v2 ->
              k
              @@
              match op with
              | Addition -> add v1 v2
              | Subtraction -> sub v1 v2
              | Multiplication -> mul v1 v2
              | Equality -> Bool (equal v1 v2)
              | Inequality -> Bool (not (equal v1 v2))
              | Less -> Bool (lt v1 v2)
              | Greater -> Bool (not (le v1 v2))
              | LessEq -> Bool (le v1 v2)
              | GreaterEq -> Bool (not (lt v1 v2))
              | _ -> failwith "unreachable"))
  | Var v -> (
      match v with
      | Name n -> k @@ Value.lookup_ident env n
      | IndexTable (tab, ind) ->
          interp_exp env ind (fun key ->
              interp_exp env tab (fun t ->
                  k
                    (match Hashtbl.find_opt (as_table t) (as_table_key key) with
                    | None -> Nil
                    | Some v -> v))))
  | FunctionDef (args, block) -> k @@ Function (Closure (args, env, block))
  | FunctionCallE fc -> interp_funcall env fc k

let run ast =
  let coroutine : (Value.tkey, Value.t) Hashtbl.t = Hashtbl.create 4 in
  Hashtbl.add coroutine (KString "create") (Value.Function CoroutCreate);
  Hashtbl.add coroutine (KString "yield") (Value.Function CoroutYield);
  Hashtbl.add coroutine (KString "mini_resume") (Value.Function CoroutResume);
  Hashtbl.add coroutine (KString "status") (Value.Function CoroutStatus);
  let globals : (string, Value.t) Hashtbl.t = Hashtbl.create 47 in
  Hashtbl.add globals "print" (Function Print);
  Hashtbl.add globals "coroutine" (Table coroutine);
  let env = Value.{ globals; locals = [] } in
  interp_block env ast (fun _ -> ()) { stat = Running (fun _ -> ()) }

open Luaparser.Ast

type value = Value.t
type env = Value.env

open Value

exception RunTimeError of string

(* let type_mismatch ?(context = "") expected got =
   raise
   @@ RunTimeError
        (Printf.sprintf "%sExpected %s, got %s"
           (if context <> "" then "Error in " ^ context else "")
           expected got) *)

(* Fonction auxiliaire pour créer une table d'environnement à partir de noms et
   valeurs associées. *)
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
let rec interp_block (env : env) (blk : block) : value =
  let env = { env with locals = create_scope blk.locals [] :: env.locals } in
  interp_stat env blk.body;
  interp_exp env blk.ret

(* Interprète une liste de statements *)

(* Interprète un statement *)
and interp_stat (env : env) (stat : stat) : unit =
  match stat with
  | Nop -> ()
  | Seq (s1, s2) ->
      interp_stat env s1;
      interp_stat env s2
  | Assign (v, e) ->
      let f =
        match v with
        | Name n -> Value.set_ident env n
        | IndexTable (tab, key) ->
            let t = interp_exp env tab |> as_table in
            Hashtbl.replace t (interp_exp env key |> as_table_key)
      in
      f (interp_exp env e)
  | FunctionCall fc -> interp_funcall env fc |> ignore
  | WhileDoEnd (e, s) ->
      while interp_exp env e |> as_bool do
        interp_stat env s
      done
  | If (e, s1, s2) ->
      if interp_exp env e |> as_bool then interp_stat env s1
      else interp_stat env s2

(* Interprète un appel de fonction *)
and interp_funcall (env : env) (fc : functioncall) : value =
  let open Value in
  let f, (args : exp list) = fc in
  let args = List.map (interp_exp env) args in
  let func = Value.as_function @@ interp_exp env f in
  match func with
  | Print ->
      List.map Value.to_string args |> String.concat "\t" |> print_endline;
      Nil
  | Closure (args_name, env, block) ->
      (* check if nb arguments match *)
      let env =
        { env with locals = create_scope args_name args :: env.locals }
      in
      interp_block env block

(* Interprète une liste d'expressions *)

(* Interprète une expression *)
and interp_exp (env : env) (e : exp) : value =
  match e with
  | Nil -> Nil
  | False -> Bool false
  | True -> Bool true
  | Integer i -> Int i
  | Float f -> Float f
  | LiteralString s -> String s
  | Table key_values ->
      let table = Hashtbl.create (List.length key_values) in
      List.iter
        (fun (k, v) ->
          Hashtbl.add table (as_table_key (interp_exp env k)) (interp_exp env v))
        key_values;
      Table table
  | UnOp (UnaryMinus, e) -> neg @@ interp_exp env e
  | UnOp (Not, e) ->
      if as_bool @@ interp_exp env e then Bool false else Bool true
  | BinOp (op, e1, e2) -> (
      let v1 = interp_exp env e1 in
      let v2 _ = interp_exp env e2 in
      match op with
      | Addition -> add v1 (v2 ())
      | Subtraction -> sub v1 (v2 ())
      | Multiplication -> mul v1 (v2 ())
      | Equality -> Bool (equal v1 (v2 ()))
      | Inequality -> Bool (not (equal v1 (v2 ())))
      | Less -> Bool (lt v1 (v2 ()))
      | Greater -> Bool (not (le v1 (v2 ())))
      | LessEq -> Bool (le v1 (v2 ()))
      | GreaterEq -> Bool (not (lt v1 (v2 ())))
      | LogicalAnd -> if as_bool v1 then v2 () else v1
      | LogicalOr -> if as_bool v1 then v1 else v2 ())
  | Var v -> (
      match v with
      | Name n -> Value.lookup_ident env n
      | IndexTable (tab, ind) -> (
          match
            Hashtbl.find_opt
              (interp_exp env tab |> as_table)
              (interp_exp env ind |> as_table_key)
          with
          | None -> Nil
          | Some v -> v))
  | FunctionDef (args, block) -> Function (Closure (args, env, block))
  | FunctionCallE fc -> interp_funcall env fc

let run ast =
  let globals = Hashtbl.create 47 in
  Hashtbl.add globals "print" (Value.Function Print);
  let env = Value.{ globals; locals = [] } in
  ignore (interp_block env ast)

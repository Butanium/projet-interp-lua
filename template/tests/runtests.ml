let tests_dir =
  match Sys.argv |> Array.to_list |> List.tl with
  | [ dir ] -> dir
  | _ ->
      Format.eprintf "usage: %s <tests_directory>\n" Sys.argv.(0);
      exit 1

let read_all_gen read cin =
  let buf = Buffer.create 4096 in
  let b = Bytes.create 4096 in
  let rec loop () =
    match read cin b 0 4096 with
    | 0 -> ()
    | n ->
        Buffer.add_subbytes buf b 0 n;
        loop ()
  in
  loop ();
  Buffer.contents buf

let read_all_channel = read_all_gen input

let run_exec_on_file exec file =
  let stdout, stdin, stderr =
    Unix.open_process_args_full exec [| exec; file |] (Unix.environment ())
  in
  let output = read_all_channel stdout in
  let err = read_all_channel stderr in
  let ret = Unix.close_process_full (stdout, stdin, stderr) = WEXITED 0 in
  (output, err, ret)

let run_lua_on_file file = run_exec_on_file "lua/mini_lua.sh" file

let run_ocaml_interp_on_file file =
  run_exec_on_file "ocaml/_build/default/interp/run.exe" file

let run_ocaml_cps_interp_on_file file =
  run_exec_on_file "ocaml/_build/default/interp-cps/run.exe" file

let run_rust_interp_on_file file =
  run_exec_on_file "rust/target/release/lua" file

type test_output = {
  output : string;
  err : string;
  res : bool;
  reference_output : string;
  reference_err : string;
  reference_res : bool;
}

let green s = "\027[1;32m" ^ s ^ "\027[0m"
let red s = "\027[1;31m" ^ s ^ "\027[0m"

let ensure_newline = function
  | "" -> ""
  | s -> if s.[String.length s - 1] = '\n' then s else s ^ "\n"

let do_tests runner testfiles =
  let runtest file : (string * bool, test_output) result =
    let reference_output, reference_err, reference_res = run_lua_on_file file in
    let output, err, res = runner file in

    if output = reference_output && res = reference_res then Ok (output, res)
    else
      Error { reference_output; reference_err; res; output; err; reference_res }
  in
  List.iter
    (fun file ->
      Printf.printf "Testing %s... %!" file;
      match runtest file with
      | Ok (_output, success) ->
          Printf.printf "%s %s\n%!" (green "[OK]")
            (if success then "" else "(failure)")
      | Error
          { output; err; res; reference_output; reference_err; reference_res }
        ->
          Printf.printf "%s\n%!" (red "[ERROR]");
          (match (res, reference_res) with
          | true, true ->
              Printf.printf "Mismatch between lua and the interpreter.\n"
          | true, false ->
              Printf.printf "Error while running the reference interpreter.\n"
          | false, true -> Printf.printf "Error while running the program.\n"
          | false, false ->
              Printf.printf
                "Errors while running the reference interpreter and the program.\n");
          Printf.printf "- Running 'lua' on the program produces:\n";

          Printf.printf "%s%s\n"
            (ensure_newline reference_output)
            (ensure_newline reference_err);
          Printf.printf "- Running the interpreter on the program produces:\n";
          Printf.printf "%s%s\n" (ensure_newline output) (ensure_newline err))
    testfiles

let () =
  let testfiles =
    Sys.readdir tests_dir |> Array.to_list
    |> List.filter (fun file -> Filename.extension file = ".lua")
    |> List.sort String.compare
    |> List.map (fun file -> Filename.concat tests_dir file)
  in
  let testfiles_no_co =
    List.filter (fun f -> not (Filename.check_suffix f ".co.lua")) testfiles
  in
  Printf.printf
    "============== Running interpreter written in OCaml  ==============\n";
  do_tests run_ocaml_interp_on_file testfiles_no_co;
  Printf.printf
    "\n============== Running interpreter written in Rust ==============\n";
  do_tests run_rust_interp_on_file testfiles_no_co;
  Printf.printf
    "\n\
     ============== Running interpreter written in OCaml (CPS variant) \
     ==============\n";
  do_tests run_ocaml_cps_interp_on_file testfiles

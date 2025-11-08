(* open Ast
open Translator
open Church_lambda

let run_test source =
  print_endline "====================================";
  print_endline ("OCaml source: " ^ source);
  try
    let structure = Ast.from_string source in
    let terms = Translator.translate_structure structure in
    print_endline "Translated lambda terms:";
    List.iter (fun term -> print_endline ("  " ^ to_lam_string term)) terms;
    print_endline "Translation succeeded.\n"
  with
  | Failure msg ->
      print_endline ("Translation failed: " ^ msg);
  | exn ->
      print_endline ("Unexpected error: " ^ Printexc.to_string exn)


let () =
  (* Test 1: Simple arithmetic expression *)
  run_test "let x = 3 + 4 in x * 2";

  (* Test 2: Function definition and application *)
  run_test "let f = fun x -> x + 1 in f 5";

  (* Test 3: If expression *)
  run_test "if true then 1 else 0";

  (* Test 4: Boolean operators *)
  run_test "let b = true && false in if b then 1 else 2";

  (* Test 5: Nested let *)
  run_test "let x = 1 in let y = 2 in x + y"; *)

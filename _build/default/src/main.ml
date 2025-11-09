open Lambda_lib

let rec lam_list_to_string lst : string=
    match lst with
    | [] -> ""
    | [x] -> Church_lambda.to_lam_string x
    | x :: xs -> Church_lambda.to_lam_string x ^ " " ^ lam_list_to_string xs

let rec list_to_string lst : string =
  match lst with
  | [] -> ""
  | [x] -> x
  | x :: xs -> x ^ " " ^ list_to_string xs

let rec read_input () =
  print_string "Enter OCaml Code to Convert to Lambda Calculus (or type QUIT to exit): ";
  flush stdout;  (* ensure prompt appears *)

  let code = read_line () in
  if String.uppercase_ascii code = "QUIT" then (
    print_endline "Exiting... REPL reduced to (λf.λx.x).";
  ) else if String.uppercase_ascii code = "FUN" then (
    print_endline "This project is inspired by Alonzo Church's Lambda Calculus. Church received an honorary degree from the University at Buffalo in 1990!";
    read_input ()
  ) else (
    (* Tokenize *)
    let tokens : string list = Church_translator.tokenize code in
    let printed_tokens = list_to_string tokens in
    print_endline ("You Entered: " ^ printed_tokens);

    (* Parse *)
    let parsed = Church_translator.parse_expr tokens in
    let lam_string = lam_list_to_string parsed in
    print_endline ("Lambda Calculus Representation: " ^ "(" ^ lam_string ^ ")");
    print_endline "----";

    (* loop again *)
    read_input ()
  );;

read_input ();;
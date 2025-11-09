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

let read_input () =
  print_endline "Enter OCaml Code to Convert to Lambda Calculus: ";
  flush stdout; (* ensure the prompt shows up *)
  let code = read_line () in

  let tokens : string list = Church_translator.tokenize code in
  let printed_tokens = list_to_string tokens in
  print_endline ("You Entered: " ^ printed_tokens);
  let parsed = Church_translator.parse_expr tokens in
  let lam_string = lam_list_to_string parsed in
  print_endline ("Lambda Calculus Representation: " ^ lam_string);

  (*print_endline "----";
  print_endline ("Enter more OCaml Code to Convert to Lambda Calculus: ");*)
  code
;;

read_input ();;
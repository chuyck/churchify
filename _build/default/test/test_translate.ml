open Lambda_lib

let rec lam_list_to_string lst : string=
    match lst with
    | [] -> ""
    | [x] -> Church_lambda.to_lam_string x
    | x :: xs -> Church_lambda.to_lam_string x ^ " ; " ^ lam_list_to_string xs

let rec list_to_string lst : string =
  match lst with
  | [] -> ""
  | [x] -> x
  | x :: xs -> x ^ " " ^ list_to_string xs

let () =
     let source = "fun y -> x + y" in
     let tokens : string list = Church_translator.tokenize source in
     let printed_tokens = list_to_string tokens in
     print_endline ("Tokens: " ^ printed_tokens);
     let parsed = Church_translator.parse_expr tokens in
     let lam_string = lam_list_to_string parsed in
     print_endline lam_string

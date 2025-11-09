open Lambda_lib

(* let rec lam_list_to_string lst : string=
    match lst with
    | [] -> ""
    | [x] -> Church_lambda.to_lam_string x
    | x :: xs -> Church_lambda.to_lam_string x ^ " " ^ lam_list_to_string xs *)

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
     let lam_string = Church_lambda.to_lam_string parsed in
     print_endline lam_string;
    (*Expected output: (λy.(x + y))*) 
    (*OCaml Equivent: fun y -> x + y*)

    let source2 = "let z = 5 in z * 2" in
    let tokens2 : string list = Church_translator.tokenize source2 in
    let printed_tokens2 = list_to_string tokens2 in
    print_endline ("Tokens: " ^ printed_tokens2);
    let parsed2 = Church_translator.parse_expr tokens2 in
    let lam_string2 = Church_lambda.to_lam_string parsed2 in
    print_endline lam_string2;
    (*Expected output: (let z = (5) in (z * (2))*) 
    (*OCaml Equivent: let z = 5 in z * 2*)  

    let source3 = "if true then 1 else 0" in
    let tokens3 : string list = Church_translator.tokenize source3 in
    let printed_tokens3 = list_to_string tokens3 in
    print_endline ("Tokens: " ^ printed_tokens3);
    let parsed3 = Church_translator.parse_expr tokens3 in
    let lam_string3 = Church_lambda.to_lam_string parsed3 in
    print_endline lam_string3;
    (*Expected output: (if (λx.λy.x) then (1) else (0)*) 
    (*OCaml Equivent: if true then 1 else 0*)

    let source4 = "fun x -> fun y -> x - y" in
    let tokens4 : string list = Church_translator.tokenize source4 in
    let printed_tokens4 = list_to_string tokens4 in
    print_endline ("Tokens: " ^ printed_tokens4);
    let parsed4 = Church_translator.parse_expr tokens4 in
    let lam_string4 = Church_lambda.to_lam_string parsed4 in
    print_endline lam_string4
    (*Expected output: (λx.λy.(x - y)*) 
    (*OCaml Equivent: fun x -> fun y -> x - y*) 

    

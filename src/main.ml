open Lambda_lib

(* Helper: print a list of strings *)
let rec list_to_string lst : string =
  match lst with
  | [] -> ""
  | [x] -> x
  | x :: xs -> x ^ " " ^ list_to_string xs

(* Main REPL loop *)
let rec repl () =
  print_string "Enter OCaml Code to Convert to Lambda Calculus (or type QUIT to exit): ";
  flush stdout;

  let input = read_line () in
  let input_upper = String.uppercase_ascii input in

  match input_upper with
  | "QUIT" ->
      print_endline "Exiting REPL. Reduced to (λf.λx.x)."
  | "FUN" ->
      print_endline "This project is inspired by Alonzo Church's Lambda Calculus. Fun Fact: Church received an honorary degree from the University at Buffalo in 1990!";
      repl ()  (* continue looping *)
  | _ ->
      (* Try to tokenize, parse, and print *)
      (try
         let tokens = Church_translator.tokenize input in
         print_endline ("You Entered: " ^ list_to_string tokens);

         let parsed = Church_translator.parse_expr tokens in
         let lam_string = Church_lambda.to_lam_string parsed in
         print_endline ("Lambda Calculus Representation: " ^ lam_string);
         print_endline "----"
       with
       | Failure msg ->
           Printf.printf "Error: %s\n----\n" msg
       | exn ->
           Printf.printf "An unexpected error occurred: %s\n----\n" (Printexc.to_string exn)
      );
      repl ()  (* loop again *)

(* Start REPL *)
let () = repl ()

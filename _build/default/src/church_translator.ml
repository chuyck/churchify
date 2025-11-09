open Stdlib
open Church_lambda

type lambda_term = Church_lambda.lambda_term

(* Lexer *)
let tokenize s : string list =
  let re = Str.regexp "\\([a-zA-Z_][a-zA-Z0-9_]*\\|[0-9]+\\|->\\|[-+*/=()]\\)" in
  let rec aux pos acc =
    if pos >= String.length s then List.rev acc
    else if Str.string_match re s pos then
      let tok = Str.matched_string s in
      aux (pos + String.length tok) (tok :: acc)
    else aux (pos + 1) acc
  in
  aux 0 []

(* Parser with operator precedence and parentheses *)
let parse_expr tokens =
  let rec parse_expr_prec min_prec toks =
    let left, toks = parse_term toks in
    parse_binop_prec left min_prec toks

  and parse_term toks =
    match toks with
  | [] -> failwith "Unexpected end"
  | "(" :: rest ->
      let inside, rest' = parse_expr_prec 0 rest in
      (match rest' with
       | ")" :: rest'' -> (inside, rest'')
       | _ -> failwith "Expected closing parenthesis")
  | "if" :: rest -> parse_if rest        (* <-- NEW CASE *)
  | "true" :: rest -> (LitBool true, rest)
  | "false" :: rest -> (LitBool false, rest)
  | "fun" :: rest -> parse_lambda rest
  | "let" :: rest -> parse_let rest
  | tok :: rest ->
      if Str.string_match (Str.regexp "^[0-9]+$") tok 0 then
        (LitInt (int_of_string tok), rest)
      else
        (Var tok, rest)

  and parse_lambda toks =
    match toks with
    | var :: "->" :: rest ->
        let body, rest' = parse_expr_prec 0 rest in
        (Lam(var, body), rest')
    | _ -> failwith "Invalid lambda"

  and parse_let toks =
    match toks with
    | var :: "=" :: rest ->
        let value, rest' = parse_expr_prec 0 rest in
        (match rest' with
         | "in" :: rest'' ->
             let body, rest''' = parse_expr_prec 0 rest'' in
             (Let(var, value, body), rest''')
         | _ -> failwith "Expected 'in'")
    | _ -> failwith "Invalid let"

  and parse_if toks =
    let cond, rest1 = parse_expr_prec 0 toks in
  match rest1 with
  | "then" :: rest2 ->
      let then_branch, rest3 = parse_expr_prec 0 rest2 in
      (match rest3 with
       | "else" :: rest4 ->
           let else_branch, rest5 = parse_expr_prec 0 rest4 in
           (If(cond, then_branch, else_branch), rest5)
       | _ -> failwith "Expected 'else'")
  | _ -> failwith "Expected 'then'"

  and parse_binop_prec left min_prec toks =
    match toks with
    | op :: rest when op = "+" || op = "-" || op = "*" || op = "/" ->
        let prec = if op = "+" || op = "-" then 1 else 2 in
        if prec < min_prec then (left, toks)
        else
          let right, rest' = parse_expr_prec (prec + 1) rest in
          let new_left = BinOp(op, left, right) in
          parse_binop_prec new_left min_prec rest'
    | _ -> (left, toks)
  in
  let result, rest = parse_expr_prec 0 tokens in
  if rest <> [] then failwith "Unexpected tokens at end"
  else result

(* Example usage *)
let () =
  let input = "let y = 10 in y + 5 * 2" in
  let tokens = tokenize input in
  let ast = parse_expr tokens in
  let lambda_str = Church_lambda.to_lam_string ast in
  Printf.printf "Input: %s\n" input;
  Printf.printf "Lambda Calculus: %s\n" lambda_str

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

(* Parser *)
let parse_expr tokens =
  let rec expr toks =
    match toks with
    | [] -> failwith "Unexpected end"
    | "true" :: rest -> (LitBool true, rest)
    | "false" :: rest -> (LitBool false, rest)
    | tok :: rest ->
      if tok = "fun" then parse_lambda rest
      else if tok = "let" then parse_let rest
      else if Str.string_match (Str.regexp "[0-9]+") tok 0 then
        (LitInt (int_of_string tok), rest)
      else
        (Var tok, rest)
  and parse_lambda toks =
    match toks with
    | var :: "->" :: rest ->
        let body, rest' = expr rest in
        (Lam(var, body), rest')
    | _ -> failwith "Invalid lambda"
  and parse_let toks =
    match toks with
    | var :: "=" :: rest ->
        let value, rest' = expr rest in
        (match rest' with
         | "in" :: rest'' ->
             let body, rest''' = expr rest'' in
             (Let(var, value, body), rest''')
         | _ -> failwith "Expected 'in'")
    | _ -> failwith "Invalid let"
  in
  let rec parse_all toks acc =
    match toks with
    | [] -> List.rev acc
    | _ ->
        let e, rest = expr toks in
        parse_all rest (e :: acc)
  in
  parse_all tokens []

(* Example usage *)
let input = "let y = 10 in y + 5"
let tokens = tokenize input
let ast_list = parse_expr tokens

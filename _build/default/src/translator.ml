(* translator.ml
open Parsetree
open Asttypes
open Church_lambda

(* Recursively translate an OCaml expression into a lambda_term *)
let rec translate_expr (e : expression) : lambda_term =
  match e.pexp_desc with
  (* Variables *)
  | Pexp_ident { txt = Longident.Lident name; _ } ->
      Var name

  (* Integer literal *)
  | Pexp_constant (Pconst_integer (s, _)) ->
      LitInt (int_of_string s)

  (* String literal *)
  | Pexp_constant (Pconst_string (s, _, _)) ->
      LitString s

  (* Boolean literals *)
  | Pexp_construct ({ txt = Longident.Lident "true"; _ }, None) ->
      LitBool true
  | Pexp_construct ({ txt = Longident.Lident "false"; _ }, None) ->
      LitBool false

  (* Lambda function: fun x -> e *)
  | Pexp_fun (_, _, { ppat_desc = Ppat_var { txt = arg; _ }; _ }, body) ->
      Lam (arg, translate_expr body)

  (* Function application: f x *)
  | Pexp_apply (fn, args) ->
      List.fold_left
        (fun acc (_, arg) -> App (acc, translate_expr arg))
        (translate_expr fn)
        args

  (* Let binding: let x = e1 in e2 *)
  | Pexp_let
      (_, [ { pvb_pat = { ppat_desc = Ppat_var { txt = name; _ }; _ }; pvb_expr } ], body) ->
      Let (name, translate_expr pvb_expr, translate_expr body)

  (* If-then-else *)
  | Pexp_ifthenelse (cond, then_e, Some else_e) ->
      If (translate_expr cond, translate_expr then_e, translate_expr else_e)

  (* Binary operator *)
  | Pexp_apply
      ({ pexp_desc = Pexp_ident { txt = Longident.Lident op; _ }; _ },
       [ (_, lhs); (_, rhs) ]) when List.mem op ["+"; "-"; "*"; "/"; "&&"; "||"; "="] ->
      BinOp (op, translate_expr lhs, translate_expr rhs)

  (* Parentheses or unit *)
  | Pexp_construct ({ txt = Longident.Lident "()"; _ }, None) ->
      LitString "()"

  (* Fallback for unsupported constructs *)
  | _ ->
      failwith "Unsupported or unimplemented OCaml construct in translator"


(* Translate an entire OCaml structure (top-level let bindings) *)
let translate_structure (s : structure) : lambda_term list =
  let rec process = function
    | [] -> []
    | { pstr_desc = Pstr_value (_, bindings); _ } :: rest ->
        let terms =
          List.map
            (fun vb ->
              match vb.pvb_pat.ppat_desc with
              | Ppat_var { txt = name; _ } ->
                  Let (name, translate_expr vb.pvb_expr, Var name)
              | _ -> translate_expr vb.pvb_expr)
            bindings
        in
        terms @ process rest
    | _ :: rest -> process rest
  in
  process s *)

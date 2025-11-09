open Parsetree
open Asttypes
open Church_lambda

(* Helper: recursively translate multiple let bindings *)
let rec translate_let_bindings bindings body =
  match bindings with
  | [] -> translate_expression body
  | { pvb_pat = { ppat_desc = Ppat_var { txt = name; _ }; _ };
      pvb_expr = expr; _ } :: rest ->
      Let (name, translate_expression expr, translate_let_bindings rest body)
  | _ -> failwith "Unsupported let binding"

and function_body_to_expression expr body : expression =
  match body with
  | Pfunction_body e -> e
  | Pfunction_cases cases _ _ ->
      (* Wrap pattern-matching cases as a match on a wildcard *)
      {
        pexp_desc = Pexp_match (
          { ppat_desc = Ppat_any; ppat_loc = expr.pexp_loc; ppat_attributes = [] },
          cases
        );
        pexp_loc = expr.pexp_loc;
        pexp_attributes = [];
      }

and translate_expression (expr : expression) : lambda_term =
  match expr.pexp_desc with
  (* Variables *)
  | Pexp_ident { txt = Longident.Lident name; _ } -> Var name

  (* Constants *)
  | Pexp_constant const ->
    (match const with
    | Pconst_integer (s, _) -> LitInt (int_of_string s)
    | Pconst_string (s, _, _) -> LitString s
    | Pconst_char c -> LitString (String.make 1 c)
    | Pconst_float (s, _) -> LitString s  (* keep original behavior *)
    | _ -> failwith "Unsupported constant")

  (* Functions *)
  | Pexp_function (params, _closed_flag, _default_expr) ->
    (match params with
    | [] -> failwith "Empty function parameters"
    | param :: rest ->
      (let name =
        (match param.pparam_desc with
        | Pparam_val (_lbl, _default_expr, { ppat_desc = Ppat_var { txt = v; _ }; _ }) -> v
        | _ -> failwith "Unsupported parameter type")
      in
      let inner_expr =
             if rest != [] then
                (* more parameters to process *)
                translate_expression { expr with
                                        pexp_desc = Pexp_function (rest, _closed_flag, _default_expr) }
             else
                (*Default*)
                translate_expression (function_body_to_expression expr param.pparam_default)
           in
           Lam(name, inner_expr)))
      (* let rec wrap_params params: lambda_term =
        match params with
        | [] -> failwith "Empty function parameters"
        | { pc_lhs = { ppat_desc = Ppat_var { txt = name; _ }; _ };
            pc_guard = _; pc_rhs = expr } :: rest ->
            let body =
              if rest = [] then translate_expression expr
              else wrap_params rest
            in
            Lam (name, body)
        | _ -> failwith "Unsupported function parameter pattern"
      in
      wrap_params params *)

  (* Operator applications first *)
  (* Operator applications only for exactly two arguments *)
  | Pexp_apply ({ pexp_desc = Pexp_ident { txt = Longident.Lident op; _ }; _ }, args) ->
      (match args with
      | [(_, lhs); (_, rhs)] -> BinOp (op, translate_expression lhs, translate_expression rhs)
      | _ -> 
          (* fallback to general function application if not exactly two args *)
          List.fold_left (fun acc (_, arg) -> App (acc, translate_expression arg))
                        (translate_expression { expr with pexp_desc = Pexp_ident { txt = Longident.Lident op; loc = expr.pexp_loc }})
                        args)


  (* General function application *)
  | Pexp_apply (func, args) ->
      List.fold_left (fun acc (_, arg) -> App (acc, translate_expression arg)) (translate_expression func) args

  (* Let bindings *)
  | Pexp_let (_rec_flag, bindings, body) ->
      translate_let_bindings bindings body

  (* If expressions *)
  | Pexp_ifthenelse (cond, then_e, else_opt) ->
      let else_e =
        match else_opt with
        | Some e -> e
        | None -> { cond with pexp_desc = Pexp_construct ({ txt = Longident.Lident "false"; loc = cond.pexp_loc }, None) }
      in
      If (translate_expression cond, translate_expression then_e, translate_expression else_e)

  (* Boolean constructors *)
  | Pexp_construct ({ txt = Longident.Lident "true"; _ }, None) -> LitBool true
  | Pexp_construct ({ txt = Longident.Lident "false"; _ }, None) -> LitBool false

  | _ -> failwith "Unsupported OCaml expression form"


(* Translate a top-level structure *)
(* let translate_structure (s : structure) : lambda_term =
  match s with
  | [{ pstr_desc = Pstr_value (_rec_flag, bindings); _ }] ->
      translate_let_bindings bindings { pexp_desc = Pexp_ident {
          txt = Longident.Lident (
            match bindings with
            | { pvb_pat = { ppat_desc = Ppat_var { txt = name; _ }; _ }; _ } :: _ -> name
            | _ -> failwith "Empty bindings"
          );
          loc = !default_loc
        } }
  | _ -> failwith "Unsupported structure form" *)

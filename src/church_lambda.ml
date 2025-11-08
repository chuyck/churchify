(*Lambda calculus representation. Structure for representing lambda terms.*)

type lambda_term =
  | Var of string                    (* Variable represented by its name *)
  | Lam of string * lambda_term      (* Lambda function with a parameter and a body *)
  | App of lambda_term * lambda_term (* Application of two functions *)
  | Let of string * lambda_term * lambda_term  (* Let binding *)
  | LitInt of int                    (* Integer literal *)
  | LitBool of bool                  (* Boolean literal *)
  | If of lambda_term * lambda_term * lambda_term  (* If expression *)
  | BinOp of string * lambda_term * lambda_term  (* Binary operation *)

let rec to_lam_string term = 
  match term with
    | Var x -> x
    | Lam (x, body) -> "(λ" ^ x ^ "." ^ to_lam_string body ^ ")"
    | App (t1, t2) -> "(" ^ to_lam_string t1 ^ " " ^ to_lam_string t2 ^ ")"
    | Let (x, t1, t2) -> "(let " ^ x ^ " = " ^ to_lam_string t1 ^ " in " ^ to_lam_string t2 ^ ")"
    | LitInt n -> "(" ^ string_of_int n ^ ")"
    | LitBool true -> "(λx.λy.x)"
    | LitBool false -> "(λx.λy.y)"
    | If (cond, then_branch, else_branch) -> (*IFTHENELSE := λp.λa.λb.p a b*)
        "(if " ^ to_lam_string cond ^ " then " ^ to_lam_string then_branch ^ " else " ^ to_lam_string else_branch ^ ")"
    | BinOp (op, t1, t2) -> (*AND := λp.λq.p q p*) (*OR := λp.λq.p p*) (*NOT := λp.p FALSE TRUE*)
        "(" ^ to_lam_string t1 ^ " " ^ op ^ " " ^ to_lam_string t2 ^ ")"


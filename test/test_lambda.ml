 let () =
    let id = Church_lambda.Lam("x", Church_lambda.Var "x") in
    let applied = Church_lambda.App(id, Church_lambda.LitInt 42) in
    print_endline(Church_lambda.to_lam_string applied);
    (*Expected output: (λx.x 42)*)
    (*OCaml Equivent: (fun x -> x) 42*)

    let boolean = Church_lambda.LitBool true in
    print_endline (Church_lambda.to_lam_string boolean);
    (*Expected output: (λx.λy.x)*)
    (*OCaml Equivent: true*)  

    let term = Church_lambda.App (Church_lambda.Lam ("x", Church_lambda.Var "x"), Church_lambda.LitInt 7) in
    print_endline (Church_lambda.to_lam_string term);
    (*Expected output: (λx.x (7))*)
    (*OCaml Equivent: (fun x -> x) 7*)

    let conditional = Church_lambda.If (Church_lambda.LitBool true, Church_lambda.LitInt 1, Church_lambda.LitInt 0) in
    print_endline (Church_lambda.to_lam_string conditional);
    (*Expected output: (if (λx.λy.x) then (1) else (0))*)
    (*OCaml Equivent: (if true then 1 else 0)*)

    let let_binding = Church_lambda.Let ("y", Church_lambda.LitInt 10, Church_lambda.BinOp ("+", Church_lambda.Var "y", Church_lambda.LitInt 5)) in
    print_endline (Church_lambda.to_lam_string let_binding);
    (*Expected output: (let y = (10) in (y + (5)))*)
    (*OCaml Equivent: let y = 10 in (y + 5)*) 

    let bin_op = Church_lambda.BinOp ("*", Church_lambda.LitInt 3, Church_lambda.LitInt 4) in
    print_endline (Church_lambda.to_lam_string bin_op);
    (*Expected output: ((3) * (4))*)
    (*OCaml Equivent: 3 * 4*) 

    let nested_app = Church_lambda.App (Church_lambda.App (Church_lambda.Lam ("x", Church_lambda.Lam ("y", Church_lambda.BinOp ("+", Church_lambda.Var "x", Church_lambda.Var "y"))), Church_lambda.LitInt 2), Church_lambda.LitInt 3) in
    print_endline (Church_lambda.to_lam_string nested_app);
    (*Expected output: ((λx.λy.(x + y) 2) 3)*)
    (*OCaml Equivent: (fun x -> fun y -> x + y) 2 3*) 

    let complex_if = Church_lambda.If (Church_lambda.BinOp (">", Church_lambda.LitInt 5, Church_lambda.LitInt 3), Church_lambda.LitBool true, Church_lambda.LitBool false) in
    print_endline (Church_lambda.to_lam_string complex_if);
    (*Expected output: (if (5 > 3) then true else false)*)
    (*OCaml Equivent: if 5 > 3 then true else false*) 
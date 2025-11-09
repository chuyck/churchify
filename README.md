# churchify
OCaml-based Lambda Calculus Abstractor.

# Project Conception:
Inspired by Alonzo Church's 1930 inceptoion of lambda calculus, this project utilizes the functional programming language OCaml to translate OCaml code into lambda calculus function abstractions.

# Run:
dune exec ./src/main.exe

Running the above command in terminal will open a text interface where a user can type a OCaml-style line of code, and in return will receive the lambda calculus translation.

# Future Plans:
->The end goal of this project is the successfully translate the entirely of the OCaml language to lambda calculus.  
->An intermediate goal of this project is the transition from parsing string versions of OCaml to the OCaml AST. This would open a path to allow for file IO, rather than REPL-based IO.  
->A future vision of this project is an investigation into compiling lambda calculus into runnning code.

# Current project scope encodes:
Booleans  
Input: true / false  
Output: (λt.λf.t) / (λt.λf.f)  
  
Positive Ints  
Input: 5  
Output: (λf.λx.(f (f (f (f (f x))))))  
  
Negative Ints  
Input: 5  
Output: - (λf.λx.(f (f (f (f (f x))))))  
  
Simple Functions  
Input: fun x -> x  
Output: (λx.x)  
  
If  
Input: if true then 1 else 0  
Output: (if ((λt.λf.t)) then ((λf.λx.(f x))) else ((λf.λx.x)))  
  
Let  
Input: let x = 1 in fun x -> x  
Output: (let x = (λf.λx.(f x)) in (λx.x))  
  

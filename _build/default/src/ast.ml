open Parsetree

let from_string (s: string) : structure =
  Parse.implementation(Lexing.from_string s)
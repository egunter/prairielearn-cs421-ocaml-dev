(* File: lambdaTest.ml *)
(* Author: Elsa L. Gunter *)
(* Copyright: 2016 *)
(* Share and Enjoy *)

open Lambda
open Lambda_parse

let rec loop () =
    (print_endline "> ";
     let tm = Lambda_parse.exp Lambda_lex.token (Lexing.from_channel stdin) in
     print_string "Evaluating:\n";
     print_endline (string_of_lambda "%" tm);
     print_endline "Using eager evalution:";
     output_eval "%" eager_one_step tm;
     print_endline "\nUsing lazy evalutation:";
     output_eval "%" lazy_one_step tm;
     print_endline "\nUsing full alpha-beta evalutation:";
     output_eval "%" beta_one_step tm;
     loop())

let _ = (print_endline "\nWelcome to the Lambda Evaluator"; loop())

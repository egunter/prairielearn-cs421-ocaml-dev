(*
  interactive-parser.ml - DO NOT EDIT
*)

open Picoml_eval
open Picomlparse

(* Try to detect if something is getting piped in *)
(*
let code =
["let a = 32 + 4;;";
"let b = a - 30;;";
"let c =\n  let a = 27\n  in\n  a - b;;";
"let f = \n  let d = \n    (a < b) && c > 15\n  in\n  let e =\n    if d then 22 else 25\n  in (d, e);;";
"let h a = a + b;;";
"let a = h b;;"]
*)

let mk_code_list code_string =
  let n = (String.length code_string) - 2 in
  let rec loop i str list =
  if i > 1
  then
    if  ((String.sub code_string (i - 2) 2) = ";;")
    then loop (i - 1) (String.sub code_string (i - 1) 1) (str :: list)
    else loop (i - 1) ((String.sub code_string (i - 1) 1) ^ str) list
  else ((String.sub code_string 0 i)^str) :: list
  in loop n (String.sub code_string n 2) []

let mem_of_code_list code_list =
  let (type_env, mem) =
  List.fold_left
    (fun (type_env, mem) -> fun s -> 
      let lexbuf = Lexing.from_string s
      in (try
            let dec_ast =
              dec 
                (fun lb ->
                  match Picomllex.token lb
                  with EOF -> raise Picomllex.EndInput
		    | r -> r)
              lexbuf 
            in
            match infer_dec gather_dec_ty_substitution type_env dec_ast
            with
                None-> (raise (Failure (s^"\ndoes not type check\n")))
              | Some (Proof(hyps,DecJudgment (_,_,new_type_env_increment))) ->
                let new_type_env = sum_env new_type_env_increment type_env
                in 
                (match eval_dec (dec_ast, mem)
                 with (result, m) ->
                   (new_type_env, m))
             | _ -> raise (Failure "This shouldn't be possible")
          with Failure err_msg -> (print_newline();
			           print_endline ("Failure:" ^ err_msg);
                                   print_newline();
                                   (type_env, mem))
            | Parsing.Parse_error -> (print_string (s^"\ndoes not parse\n");
                                      (type_env, mem))
      )
    )
    ([], [])
    code_list
  in mem

let compare_mem is_html score stu_mem comp_mem =
  let stu_cmem = canonicalize_memory stu_mem in
  let comp_cmem = canonicalize_memory comp_mem in
  if (stu_cmem = comp_cmem)
  then (true, string_of_memory is_html stu_cmem, string_of_memory is_html comp_cmem, score)
  else (false, string_of_memory is_html stu_cmem, string_of_memory is_html comp_cmem, 0)

let mem_from_string answer =
  let lexbuf = Lexing.from_string answer
  in environment
            (fun lb ->
              match Picomllex.token lb
              with EOF -> raise Picomllex.EndInput
	        | r -> r)
            lexbuf

let score_per_part = 2

let checkAnswer (code_string, answer) =
  try
    let code_list = mk_code_list code_string in
    (try
       let comp_mem = mem_of_code_list code_list in
       (try
          let student_mem = mem_from_string answer in
          compare_mem false score_per_part student_mem comp_mem
        with _ -> (false, ("Parse error in: "^answer),
                   string_of_memory false comp_mem, 0))
     with _ -> (false, answer,
                "System error -- Please nofity staff; failure to evaluate given code.", 0))
  with _ -> (false, answer,
                "System error -- Please nofity staff; Given ocaml code snippet failed to parse or typecheck.", 0)


(*
let _ = checkAnswer ("let a = 32 + 4;; let b = a - 30;;","{ a -> 36, b -> 6 }")
*)

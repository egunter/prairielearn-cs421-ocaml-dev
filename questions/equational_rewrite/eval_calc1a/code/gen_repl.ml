(*
File: gen_repl.ml
Author: Elsa L Gunter
Share and Enjoy
*)

(* 
Assumes eval_input_in_state returns a pair of a state and a flag as to whether
the dialog is over.
*)

let rec read_loop str_buff =
  let s = read_line() in
  let s_len_minus_1 = ((String.length s) - 1) in
  match String.get s s_len_minus_1 
  with '.' -> str_buff ^ (String.sub s 0 s_len_minus_1) (*^ "\n"*)
    | _ -> read_loop (str_buff ^ s ^ "\n")

let read_eval_print_loop
(*    string_of_state
    parse_input *)
    eval_input_in_state
    prompt
    intro_text
    intro_state =
  let _ = print_endline intro_text in
  let rec loop state =
    let _ = (print_string prompt; flush stdout) in
    try
      let student_input = read_loop "" in
(*
We should evaluate the input string in the context a state, which should
return a new state, and a boolean flag saying whether we are done.  If we
are done, we return the state, and otherwise we call the loop with the
new state.
We will leave all printing of the state and pre-prompt instructions to the
eval function.
*)
      let (new_state, are_done) = eval_input_in_state state student_input in
      if are_done then (new_state, true) else loop new_state
    with _ -> (state, false)
  in loop intro_state

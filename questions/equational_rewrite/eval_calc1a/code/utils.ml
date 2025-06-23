(* 
File: utils.ml 
Author: Elsa L Gunter
Share and Enjoy
*)

(* Util functions *)
let rec drop y = function
   []    -> []
 | x::xs -> if x=y then drop y xs else x::drop y xs

let rec delete_duplicates = function
   []    -> []
 | x::xs -> x::delete_duplicates (drop x xs)


(* Format for file recording the student's responses *)

type input = {step_num : int; this_step_attempt_num : int; prompt: string; student_string: string}

(*
type student_inputs = {cite: string; inputHistory: input list}
*)

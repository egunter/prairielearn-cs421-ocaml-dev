(* 
File: gen_env.ml 
Author: Elsa L Gunter
Share and Enjoy
*)

(* environments *)
type 'a env = (string * 'a) list

let string_of_seq string_of_elt lbrack sep rbrack seq =
  let rec string_of_seq_aux seq =
    match seq
    with [] -> ""
      | (x::xs) -> sep^" "^(string_of_elt x)^(string_of_seq_aux xs)
  in match seq with [] -> lbrack ^ rbrack
    | x :: xs -> lbrack^(string_of_elt x)^(string_of_seq_aux xs)^rbrack

let string_of_env string_of_entry mapsto_sym sep gamma = 
  string_of_seq
    (fun (key,value) -> (key ^ " "^ mapsto_sym ^ " " ^ string_of_entry value))
    "{" sep "}"
    gamma

(*environment operations*)
let rec lookup mapping x =
  match mapping with
     []        -> None
   | (y,z)::ys -> if x = y then Some z else lookup ys x

let make_env x y = ([(x,y)]:'a env)
let lookup_env (gamma:'a env) x = lookup gamma x
let ins_env (gamma:'a env) x y = 
  (List.merge (fun (a,_) (b,_) -> String.compare a b) [(x,y)]
     (List.filter (fun (u,v) -> not(x = u)) gamma))
let sum_env (delta:'a env) (gamma:'a env) = 
  List.fold_right (fun (x,y) env -> ins_env env x y) delta gamma


let canonicalize_env canonicalize_value env =
  List.fold_right
    (fun (var,value) -> fun canon_env ->
      List.merge
        (fun (v1,_) -> fun (v2,_) -> String.compare v1 v2)
        [(var, canonicalize_value value)]
        canon_env)
    env
    []
    
let missing_value_error_message value_to_string (key, desired_value) =
  ("The input environment has no entry for \""^key^
      "\", when it should have any entery of ("^
      (value_to_string desired_value) ^").")

let excess_value_error_message value_to_string (key, excess_value) =
  ("The input environment has an entry of ("^
      (value_to_string excess_value) ^ ") for the key \""^key^
      "\", when it should have none.")

let differing_value_error_message value_to_string (key, (wrong_value, right_value)) =
  ("The input environment has an entry of ("^ (value_to_string wrong_value) ^
      ") for the key \""^key^"\", when it should have any entery of ("^
      (value_to_string right_value) ^").")

let cumulative_missing_value_error
    value_to_string 
    missing_penalty
    accumulate_missing_penalty
    (num_missing_errors, cumulative_missing_penalty, cumulative_missing_messages)
    (key, desired_value)
    =
  let new_number_missing_errors = num_missing_errors + 1
  in
  (new_number_missing_errors,
   accumulate_missing_penalty missing_penalty cumulative_missing_penalty,
   ((missing_value_error_message value_to_string (key, desired_value)) ^
   ",\n" ^ cumulative_missing_messages))

let cumulative_excess_value_error
    value_to_string 
    excess_penalty
    accumulate_excess_penalty
    (num_excess_errors, cumulative_excess_penalty, cumulative_excess_messages)
    (key, desired_value) =
  let new_number_excess_errors = num_excess_errors + 1
  in
  (new_number_excess_errors,
   accumulate_excess_penalty excess_penalty cumulative_excess_penalty,
   ((excess_value_error_message value_to_string (key, desired_value)) ^
   ",\n" ^ cumulative_excess_messages))

let cumulative_differing_value_error
    value_to_string 
    differing_penalty
    accumulate_differing_penalty
    (num_differing_errors, cumulative_differing_penalty, cumulative_differing_messages)
    (key, (wrong_value, right_value)) =
  let new_number_differing_errors = num_differing_errors + 1
  in
  (new_number_differing_errors,
   accumulate_differing_penalty differing_penalty cumulative_differing_penalty,
   ((differing_value_error_message value_to_string (key, (wrong_value, right_value))) ^
   ",\n" ^ cumulative_differing_messages))
       

let cumulative_env_errors
    value_to_string 
    missing_penalty
    accumulate_missing_penalty
    excess_penalty
    accumulate_excess_penalty
    differing_penalty
    accumulate_differing_penalty
    input_env
    correct_env =
  let rec cum_err
      (me_info, ee_info, de_info)
      in_env
      cor_env =
    match (in_env, cor_env)
    with ([],_) ->
      let new_me_info =
        List.fold_left
          (cumulative_missing_value_error
             value_to_string missing_penalty accumulate_missing_penalty)
          me_info
          cor_env
      in
      (new_me_info, ee_info, de_info)
      | (_,[]) ->
        let new_ee_info =
          List.fold_left
            (cumulative_excess_value_error
                value_to_string excess_penalty accumulate_excess_penalty)
            ee_info
            in_env
        in
        (me_info, new_ee_info, de_info)
      | ((ik,iv)::rem_in_env, (ck,cv)::rem_cor_env) ->
        if ik > ck
        then
          let new_me_info =
            cumulative_missing_value_error
              value_to_string
              missing_penalty
              accumulate_missing_penalty
              me_info
              (ck,cv)
          in 
          cum_err (new_me_info, ee_info, de_info) in_env rem_cor_env
        else if ik < ck
        then
          let new_ee_info =
            cumulative_excess_value_error
              value_to_string
              excess_penalty
              accumulate_excess_penalty
              ee_info
              (ik,iv)
          in
          cum_err (me_info, new_ee_info, de_info) rem_in_env cor_env
        else if iv = cv
        then cum_err (me_info, ee_info, de_info) rem_in_env rem_cor_env
        else
          let new_de_info =
            cumulative_differing_value_error
              value_to_string 
              differing_penalty
              accumulate_differing_penalty
              de_info
              (ik,(iv,cv))
          in cum_err (me_info, ee_info, new_de_info) rem_in_env rem_cor_env
  in cum_err ((0,0,""),(0,0,""),(0,0,"")) input_env correct_env



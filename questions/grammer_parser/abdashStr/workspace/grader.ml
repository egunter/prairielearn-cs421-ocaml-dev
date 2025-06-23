(* File: grader.ml *)
(* Author: Elsa L. Gunter *)
(* Copyright: 2017 *)
(* Share and Enjoy *)

open Regexgrader
open RrgGrader

let total_string s m ="\nTotal: [" ^ (string_of_int s) ^ " / " ^ (string_of_int m) ^ "]\n\nExtra: [0 / 0]\n"

let _ =
  let (regexp_report, regexp_score, regexp_max) = regexp_results in
  let (rrg_report, rrg_score, rrg_max) = rrg_results in
  print_string regexp_report;
  print_string "\n---------------------------------------------------------------\n\n";
  print_string rrg_report;
  print_string ("\n" ^ total_string (regexp_score + rrg_score) (regexp_max + rrg_max))

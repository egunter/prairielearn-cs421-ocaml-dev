open Genutils;;

let tree = [
("root-l0a", {str_label  = "App"; str_left  = "{y : int}"; str_middle = "(fun x -> x + y) (~ 5)"; str_right  = "int"; str_sideCondition = ""});
("root-l0a-l1a", {str_label  = "Fun";str_left = "{y: int}";str_middle = "fun x -> x + y";str_right = "int -> int";str_sideCondition = ""});
("root-l0a-l1a-l2a", {str_label  = "BinOp";str_left = "{x: int, y: int}";str_middle = "x + y";str_right = "int";str_sideCondition = ""});
("root-l0a-l1a-l2a-l3a", {str_label  = "Var";str_left = "{x: int, y: int}";str_middle = "x";str_right  = "int";str_sideCondition = ""});
("root-l0a-l1a-l2a-l3b", {str_label  = "Var";str_left = "{x: int, y: int}";str_middle = "y";str_right  = "int";str_sideCondition = ""});
("root-l0a-l1b", {str_label  = "MonOp";str_left   = "{y: int}";str_middle = "~5";str_right = "int";str_sideCondition = ""});
("root-l0a-l1b-l2a", {str_label  = "Const";str_left   = "{y: int}";str_middle = "5";str_right  = "int";str_sideCondition = ""});
]

(*
    "Const"
  | "Var"
  | "MonOp"
  | "BinOp"
  | "If"
  | "App"
  | "Fun"
  | "Let"
  | "LetRec"
*)
{

open Parse
open Common

exception EndInput

}

(* You can assign names to commonly-used regular expressions in this part
   of the code, to save the trouble of re-typing them each time they are used *)
let numeric = ['0' - '9']
let lower = ['a' - 'z']
let upper = ['A' - 'Z']
let alpha = ['a' - 'z' 'A' - 'Z' ]
let alphanum = alpha | numeric
let id = lower (alphanum "_" )*
let whitespace = [' ' '\t' '\n']

rule token = parse
  | whitespace { token lexbuf }  (* skip over whitespace *)
  | eof             { EOF }

  | "["     { LBRAC  }
  | "]"     { RBRAC  }
  | "{"     { LBRACE }
  | "}"     { RBRACE }
  | "("     { LPAREN  }
  | ")"     { RPAREN  }
  | ","     { COMMA  }
  | "->"    { ARROW  }

  | "+"     { PLUS  }
  | "-"     { MINUS  }
  | "*"     { TIMES  }

  | "skip"  { SKIP }
  | ";"     { SEMICOLON }
  | ":="    { ASSIGN }
  | "if"    { IF  }
  | "then"  { THEN  }
  | "else"  { ELSE  }
  | "fi"    { FI }
  | "while" { WHILE }
  | "do"    { DO }
  | "od"    { OD }

  | "true"  { TRUE }
  | "false" { FALSE }
  | "&"     { AND }
  | "or"    { OR }
  | "not"   { NOT  }
  | "<"     { LT  }
  | "="     { EQUALS  }

  | numeric+ as s { INT (int_of_string s) }
  | id as s     { IDENT s }


(* do not modify this function: *)
{
let lextest s = token (Lexing.from_string s)

let get_all_tokens s =
    let b = Lexing.from_string (s^"\n") in
    let rec g () =
        match token b with
            EOF -> []
            | t -> t :: g ()
        in
    g ()

let get_all_token_options s =
    let b = Lexing.from_string (s^"\n") in
    let rec g () =
        match (try Some (token b) with _ -> None) with
            Some EOF -> []
            | None -> [None]
            | t -> t :: g ()
        in
    g ()


 }

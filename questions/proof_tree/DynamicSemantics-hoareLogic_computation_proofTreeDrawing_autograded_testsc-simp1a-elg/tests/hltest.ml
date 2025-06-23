open Common

(* helper functions *)
let insert a l = if List.exists (fun x -> if x = a then true else false) l then l else a::l;;

let rec insertAll l1 l2 = match l1 with [] -> l2 | x::xl -> insertAll xl (insert x l2);;

let rec collectVarsExp x = match x with IntConst a -> []
           | Ident a -> [a]
           | MonOpAppExp (a,b) -> collectVarsExp b
           | BinOpAppExp (a,b,c) -> insertAll (collectVarsExp b) (collectVarsExp c);;

let rec collectVars b = match b with BoolConst x -> []
       | AndExp (x,y) -> insertAll (collectVars x) (collectVars y)
       | OrExp (x, y) -> insertAll (collectVars x) (collectVars y)
       | NotExp x -> collectVars x
       | LessExp (x,y) -> insertAll (collectVarsExp x) (collectVarsExp y)
       | LessEqExp (x,y) -> insertAll (collectVarsExp x) (collectVarsExp y)
       | GreaterExp (x,y) -> insertAll (collectVarsExp x) (collectVarsExp y)
       | GreaterEqExp (x,y) -> insertAll (collectVarsExp x) (collectVarsExp y)
       | EqExp (x,y) -> insertAll (collectVarsExp x) (collectVarsExp y)
       | ImpExp (x,y) -> insertAll (collectVars x) (collectVars y);;

let rec lookup m x = match m with [] -> None
         | ((a,b)::xl) -> if a = x then Some b else lookup xl x;;

(* first set up range *)
let minnum = -20
let maxnum = 20

let rec initialNum l = match l with [] -> []
        | x::xl -> (x,minnum)::(initialNum xl);;

let rec getNextTest l = match l with [] -> None
        | (x,y)::xl -> if y = maxnum then
          (match getNextTest xl with None -> None
               | Some xl' -> Some ((x,minnum)::xl'))
          else Some ((x,y+1)::xl);;

(*  do one test *)
let rec getNum x l = match l with [] -> None
             | (a,b)::al -> if x = a then Some b else getNum x al;;

let interBinOp op x y = match op with IntPlusOp -> x + y
     | IntMinusOp -> x - y | IntTimesOp -> x * y | IntDivOp -> x / y
     | ModOp -> x mod y;;

let rec interpretExp env e = match e with IntConst a -> a
         | Ident x -> (match getNum x env with None -> raise (Failure "bad key.")
            | Some a -> a)
          | MonOpAppExp (IntNegOp, x) -> - (interpretExp env x)
          | BinOpAppExp (op, x, y) ->  interBinOp op (interpretExp env x) (interpretExp env y);;

let rec interpretRel env e = match e with BoolConst b -> b
   | AndExp (x,y) -> (interpretRel env x) && (interpretRel env y)
   | OrExp (x,y) -> (interpretRel env x) || (interpretRel env y)
   | NotExp x -> not (interpretRel env x)
   | ImpExp (x,y) -> (not (interpretRel env x)) || (interpretRel env y)
   | LessExp (x,y) -> (interpretExp env x) < (interpretExp env y)
   | GreaterExp (x,y) -> (interpretExp env x) > (interpretExp env y)
   | LessEqExp (x,y) -> (interpretExp env x) <= (interpretExp env y)
   | GreaterEqExp (x,y) -> (interpretExp env x) >= (interpretExp env y)
   | EqExp (x,y) -> (interpretExp env x) = (interpretExp env y)

let rec env_to_string env = match env with
  | [] -> ""
  | (x, a) :: t -> x ^ " -> " ^ (string_of_int a) ^ ", " ^ (env_to_string t)

(* third, do the test solving *)
let rec validTest env e = if interpretRel env e then 
        (match getNextTest env with None -> true
                | Some env' -> validTest env' e)
        else false;;

let validCheck b = let l = collectVars b
          in let theMap = initialNum l in 
            validTest theMap b;;

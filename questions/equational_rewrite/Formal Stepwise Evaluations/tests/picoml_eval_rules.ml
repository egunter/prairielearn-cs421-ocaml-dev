(* File: picoml_eval.ml *)
(* Author: Elsa L. Gunter *)
(* Share and Enjoy *)

let eval_rules =
[
  ("EvalConst", "Eval(const, env) => Val const");
  ("EvalVar", "Eval(var, env) => Val(env var) --- if var is in the domain of env");
  ("EvalPair", "Eval((Val v1, Val v2), env) => Val (v1,v2)");
  ("EvalPairFst", "Eval((exp1, Val v2), env) => Eval((Eval(exp1, env), Val v2), env)");
  ("EvalPairSnd", "Eval((exp1,exp2), env) => Eval((exp1, Eval(exp2, env)), env)");
  ("EvalPrimOp", "Eval((Val v1) op (Val v2), env) => Val (v1 op v2)");
  ("EvalPrimOpL",
   "Eval(exp1 op (Val v2), env) => Eval((Eval(exp1, env) op (Val v2)), env)");
  ("EvalPrimOpR", "Eval(exp1 op exp2, env) => Eval(exp1 op (Eval(exp2, env)), env)");
  ("EvalIfTrue", "Eval(if Val true then exp_t else exp_f, env) => Eval(exp_t, env)");
  ("EvalIfFalse", "Eval(if Val false then exp_t else exp_f, env) => Eval(exp_f, env)");
  ("EvalIf", "\tEval(if exp_b then exp_t else exp_f, env) =>\n"^
    "       \t\t  Eval(if Eval(exp_b, env) then exp_t else exp_f, env)");
  ("EvalApp",
   "Eval ((Val <x -> exp_b, env_f>) (Val v), env) => Eval (exp_b {x -> v} + env_f)");
  ("EvalAppFun", "Eval (exp_f (Val va), env) => Eval (Eval(exp_f, env) (Val va), env)");
  ("EvalAppArg", "Eval(exp_f exp_a, env) => Eval(exp_f (Eval(exp_a, env)), env)");
  ("EvalFun", "Eval(fun x -> exp, env) => Val <x -> exp, env>");
  ("EvalLet", "Eval(let x = Val v in exp2, env) => Eval(exp2, {x -> v} + env)");
  ("EvalLetBinding",
   "Eval(let x = exp1 in exp2, env) => Evap(let x = Eval(exp1, env) in exp2, env)")
]

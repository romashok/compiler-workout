open GT
open Language
open List

(* The type for the stack machine instructions *)
@type insn =
(* binary operator                 *) | BINOP of string
(* put a constant on the stack     *) | CONST of int
(* read to stack                   *) | READ
(* write from stack                *) | WRITE
(* load a variable to the stack    *) | LD    of string
(* store a variable from the stack *) | ST    of string with show

(* The type for the stack machine program *)
type prg = insn list

(* The type for the stack machine configuration: a stack and a configuration from statement
   interpreter
 *)
type config = int list * Language.Stmt.config

(* Stack machine interpreter

     val eval : config -> prg -> config

   Takes a configuration and a program, and returns a configuration as a result
 *)
let rec eval cfg prg =
  let rec eval_step (stack, ((state, input, output) as cfg)) cmd =
    match cmd with
      | READ     -> (hd input :: stack, (state, tl input, output))
      | WRITE    -> (tl stack, (state, input, output @ [hd stack]))
      | CONST n  -> (n :: stack, cfg)
      | LD x     -> (state x :: stack, cfg)
      | ST x     ->
          let state' = Language.Expr.update x (hd stack) state in
            (tl stack, (state', input, output))
      | BINOP op ->
          let rhs :: lhs :: stack' = stack in
            (Language.Expr.opByName op lhs rhs :: stack', cfg) in

  match prg with
    | [] -> cfg
    | cmd :: prg' -> eval (eval_step cfg cmd) prg'

(* Top-level evaluation

     val run : prg -> int list -> int list

   Takes a program, an input stream, and returns an output stream this program calculates
*)
let run p i = let (_, (_, _, o)) = eval ([], (Language.Expr.empty, i, [])) p in o

(* Stack machine compiler

     val compile : Language.Stmt.t -> prg

   Takes a program in the source language and returns an equivalent program for the
   stack machine
 *)
let rec compile stmt =
  let rec compile_expr expr =
    match expr with
      | Language.Expr.Const n              -> [CONST n]
      | Language.Expr.Var x                -> [LD x]
      | Language.Expr.Binop (op, lhs, rhs) -> compile_expr lhs @ compile_expr rhs @ [BINOP op] in

  match stmt with
    | Language.Stmt.Read x           -> [READ; ST x]
    | Language.Stmt.Write expr       -> compile_expr expr @ [WRITE]
    | Language.Stmt.Assign (x, expr) -> compile_expr expr @ [ST x]
    | Language.Stmt.Seq (s1, s2)     -> compile s1 @ compile s2
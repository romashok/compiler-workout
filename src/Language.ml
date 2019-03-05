(* Opening a library for generic programming (https://github.com/dboulytchev/GT).
   The library provides "@type ..." syntax extension and plugins like show, etc.
*)
open GT

(* Opening a library for combinator-based syntax analysis *)
open Ostap.Combinators

(* Simple expressions: syntax and semantics *)
module Expr =
  struct

    (* The type for expressions. Note, in regular OCaml there is no "@type..."
       notation, it came from GT.
    *)
    @type t =
    (* integer constant *) | Const of int
    (* variable         *) | Var   of string
    (* binary operator  *) | Binop of string * t * t with show

    (* Available binary operators:
        !!                   --- disjunction
        &&                   --- conjunction
        ==, !=, <=, <, >=, > --- comparisons
        +, -                 --- addition, subtraction
        *, /, %              --- multiplication, division, reminder
    *)

    (* State: a partial map from variables to integer values. *)
    type state = string -> int

    (* Empty state: maps every variable into nothing. *)
    let empty = fun x -> failwith (Printf.sprintf "Undefined variable %s" x)

    (* Update: non-destructively "modifies" the state s by binding the variable x
      to value v and returns the new state.
    *)
    let update x v s = fun y -> if x = y then v else s y

    (* Expression evaluator

          val eval : state -> t -> int

       Takes a state and an expression, and returns the value of the expression in
       the given state.
    *)
    let opByName op =
      let to_int b = if b then 1 else 0 in
      let to_bool n = n != 0 in
      let from_relational op = fun lhs rhs -> to_int (op lhs rhs) in
      let from_logical op = fun lhs rhs -> to_int (op (to_bool lhs) (to_bool rhs)) in

      match op with
        | "+"  ->                 ( + )
        | "-"  ->                 ( - )
        | "*"  ->                 ( * )
        | "/"  ->                 ( / )
        | "%"  ->                 ( mod )
        | "<"  -> from_relational ( <  )
        | "<=" -> from_relational ( <= )
        | ">"  -> from_relational ( >  )
        | ">=" -> from_relational ( >= )
        | "==" -> from_relational ( == )
        | "!=" -> from_relational ( != )
        | "&&" -> from_logical    ( && )
        | "!!" -> from_logical    ( || )
        | _    -> failwith (Printf.sprintf "Unknown operator: " ^ op)

    let rec eval s expr =
      match expr with
        | Const n              -> n
        | Var x                -> s x
        | Binop (op, lhs, rhs) -> opByName op (eval s lhs) (eval s rhs)

    (* Expression parser. You can use the following terminals:

         IDENT   --- a non-empty identifier a-zA-Z[a-zA-Z0-9_]* as a string
         DECIMAL --- a decimal constant [0-9]+ as a string

    *)
    ostap (
      parse: empty {failwith "Not implemented yet"}
    )

  end

(* Simple statements: syntax and sematics *)
module Stmt =
  struct

    (* The type for statements *)
    @type t =
    (* read into the variable           *) | Read   of string
    (* write the value of an expression *) | Write  of Expr.t
    (* assignment                       *) | Assign of string * Expr.t
    (* composition                      *) | Seq    of t * t with show

    (* The type of configuration: a state, an input stream, an output stream *)
    type config = Expr.state * int list * int list

    (* Statement evaluator

          val eval : config -> t -> config

       Takes a configuration and a statement, and returns another configuration
    *)
    let rec eval ((state, input, output) as cfg) stmt =
      match stmt with
        | Assign (x, expr) ->
            let v = (Expr.eval state expr) in
            let state' = Expr.update x v state in
              (state', input, output)
        | Read x ->
            let state' = Expr.update x (hd input) state in
              (state', tl input, output)
        | Write e ->
            let v = Expr.eval state e in
              (state, input, v :: output)
        | Seq (s1, s2) ->
            let c  = eval cfg s1 in
              eval c s2

    (* Statement parser *)
    ostap (
      parse: empty {failwith "Not implemented yet"}
    )

  end

(* The top-level definitions *)

(* The top-level syntax category is statement *)
type t = Stmt.t

(* Top-level evaluator

     eval : t -> int list -> int list

   Takes a program and its input stream, and returns the output stream
*)
let eval p i =
  let _, _, o = Stmt.eval (Expr.empty, i, []) p in o

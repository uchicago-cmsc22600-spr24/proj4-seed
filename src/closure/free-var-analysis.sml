(* free-var-analysis.sml
 *
 * COPYRIGHT (c) 2024 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2024
 * University of Chicago
 *
 * An analysis pass that computes the free variables of functions.
 *)

structure FreeVarAnalysis : sig

    (* Analyze a SimpleAST program to compute the free variables of functions
     * and the live variables at splits (if-then-else expressions) and joins
     * (let expressions with rhs expressions).
     *)
    val analyze : SimpleAST.program -> unit

    (* return the free variables of a function; the result is only valid after the
     * `analyze` function has been run.
     *)
    val freeVarsOf : SimpleVar.t -> SimpleVar.t list

    (* is the free-variables set of a function empty *)
    val hasNoFreeVars : SimpleVar.t -> bool

  end = struct

    structure S = SimpleAST
    structure V = SimpleVar
    structure Set = V.Set

    (* variable property to record the free variables of functions *)
    local
      val {getFn, setFn, ...} = V.newProp (fn _ => Set.empty)
    in
    val getFreeVars = getFn
    val setFreeVars = setFn
    val freeVarsOf = Set.toList o getFn
    fun hasNoFreeVars x = Set.isEmpty(getFreeVars x)
    end (* local *)

    (* compute the free variables of the functions in the program. *)
    fun freeVars e = () (* YOUR CODE HERE *)

    (* free vars analysis *)
    fun analyze (S.PROG(_, e)) = freeVars e

  end

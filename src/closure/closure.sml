(* closure.sml
 *
 * COPYRIGHT (c) 2024 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2024
 * University of Chicago
 *
 * Convert the higher-order SimpleAST IR to the first-order SimpleAST IR.
 *)

structure Closure : sig

    (* transform the higher-order SimpleAST program to a first-order
     * SimpleAST program by applying closure conversion.  We assume that
     * the free variable analysis has already been run on the program.
     *)
    val transform : SimpleAST.program -> SimpleAST.program

  end = struct

    structure S = SimpleAST
    structure SV = SimpleVar
    structure VMap = SV.Map
    structure VSet = SV.Set
    structure FVA = FreeVarAnalysis

    (********** Renaming environment **********)

    (* the environment tracks the renaming of free variables and the set of
     * fun-bound variables that are currently in scope.
     *)
    type env = {
        rn : SV.t VMap.map,
        fns : VSet.set
      }

    (* the empty environment *)
    val empty : env = {rn = VMap.empty, fns = VSet.empty}

    (* rename a variable *)
    fun rename (env : env, x) = (case VMap.find(#rn env, x)
           of SOME y => y
            | NONE => x
          (* end case *))

    (* insert a renaming for a free variable into the environment *)
    fun insertFV (x, y, {rn, fns}) : env = {rn = VMap.insert(rn, x, y), fns=fns}

    (* record a that `f` is bound *)
    fun bindFun ({rn, fns}, f) : env = {rn = rn, fns = VSet.add(fns, f)}

    (* `isFunBound (env, f)` returns true if `f` is a fun-bound variable in scope *)
    fun isFunBound ({rn, fns}, f) = VSet.member(fns, f)

    (* return true if `f` is a known function (we assume that it is fun-bound) *)
    fun isKnown f = (SV.useCntOf f = Census.appCntOf f)

    (* a property for mapping functions to the name of their first-order counterpart *)
    val {getFn=firstOrderFunOf, ...} =
          SV.newProp (fn f => SV.new(SV.nameOf f ^ "_code", PrimType.CODE))

    (********** Conversion **********)

    (* convert a SimpleAST variable; the result will be a value *)
    fun cvtVar env x = S.V_VAR(rename (env, x))

    (* convert a SimpleAST value *)
    fun cvtVal env v = (case v
           of S.V_VAR x => S.V_VAR(rename (env, x))
            | v => v
          (* end case *))

    (* convert a list of SimpleAST values *)
    fun cvtVals (env, vs) = List.map (cvtVal env) vs

    fun cvtExp (env, S.E(ppt, e)) = let
          fun mk e = S.E(ppt, e)
          in
            (* YOUR CODE HERE *)
          end

    and cvtRHS (env, rhs) = (* YOUR CODE HERE *)

    and cvtRule env (p, e) = (p, cvtExp(env, e))

    (* run the analysis and closure conversion on the program *)
    fun transform (S.PROG(vArgs, body)) = S.PROG(vArgs, cvtExp(empty, body))

  end

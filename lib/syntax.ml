open Support.Error

(* ---------------------------------------------------------------------- *)
(* Datatypes *)

type term =
    Predicate of predicate (* or functor operator *)
  | Variable of info * string * int
 (* int part is unique and indentical for all appearances of each variable.
    it avoids capturing free variables with identical names by giving them different indices
    but giving bound variables identical indices as in Barendregt's variable convention *)

and args = term list

and predicate = {
  atom: info * string * int; (* same as Variable *)
  args: args
}

type conjunction = term list

type disjunction = conjunction list

type goal = disjunction

type query = goal

type fact = term (* definitley Predicate, because of parsing rules *)

type rule = (term * goal)  (* also definitley Predicate *)

type clause = 
    Rule of rule
  | Fact of fact

type program = clause list
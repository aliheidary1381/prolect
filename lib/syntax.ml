open Support.Error

let first_free_index = ref 2 (* 0 & 1 are reserved for false & true *)

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
  atom: info * string * int; (* int part same as Variable *)
  args: args
}

type conjunction = term list

type disjunction = conjunction list

type goal = disjunction

type query = goal

type fact = term (* definitley Predicate, because of parsing rules *)

type rule = (term * goal)  (* also definitley Predicate *)

type clause = (* or relation *)
    Rule of rule
  | Fact of fact

type program = clause list

module IntMap = Map.Make(Int)
type substitution = term IntMap.t (* Barendregt index of Variables -> ground terms (Predicates) *)
(* true & false's index are assumed to be 1 & 0 *)
let empty_sub: substitution = IntMap.empty

type solution_set = substitution Seq.t
let _true: solution_set = Seq.return empty_sub (* meaning there is no need to substitute anything for goal to be satisfied i.e. there are no variables *)
and _false: solution_set = Seq.empty (* meaning there is no substitution that satisfies goal *)

module Substitution = struct
  (*
  If X was substituted to Y and Y to a, substitution_find X will return a
  *)
  let rec find (key: int) (sub: substitution ref) : term = (* works like the find operation in disjoint set data structure *)
    let value = IntMap.find key !sub in
    match value with
      Predicate(_) -> value
    | Variable(_, _, i) ->
      try begin
        let anc = sub |> find i in
        sub := !sub |> IntMap.update key (Option.map (fun _ -> anc));
        anc
      end with Not_found -> value

  let add (key: int) (value: term) (sub: substitution) : substitution =
    IntMap.add key value sub

  (*
  uses a substitution to make a term more ground. e.g.:
  sub = X -> c
  term = a(Y, b(X, d))
  return = a(Y, b(c, d))
  *)
  let rec substitute (sub: substitution ref) (term: term) : term = match term with
      Variable(_, _, i) -> (try sub |> find i with Not_found -> term)
    | Predicate({atom; args}) -> Predicate({atom; args=args |> List.map (substitute sub)})

  (*
  uses a substitution to make another substitution more ground. e.g.:
  sub_out = X -> Y, Z -> W
  sub_in = Y -> c
  return = X -> c, Z -> W
  *)
  let sub_sub (sub_out: substitution) (sub_in': substitution) : substitution =
    let sub_in = ref(sub_in') in
    sub_out |> IntMap.map (substitute sub_in)
end

module Simulation = struct
  (* The Barendregt's convention falls short when a single clause
    recursively calls itself, because the same variable will be
    bound to multiple values on each call in the stack trace.
    An easy solution is to just pretend that each recursion simply
    calls a new copy of the same functor/clause.
    Simulation of variables is done on the fly once every two recursive call.
  *)
  let rec is_simulated = function
      Variable(_, _, i) -> (i >= !first_free_index)
    | Predicate({atom=_; args}) -> List.exists is_simulated args

  let rec simulate_trm = function
      Variable(a, b, c) -> Variable(a, b, c + !first_free_index)
    | Predicate({atom; args}) -> Predicate({atom; args=List.map simulate_trm args})

  let simulate (q: query): query =
    List.map (fun conj -> List.map simulate_trm conj) q
end

open Syntax
open Printing
open Syntax.Simulation

let prog = ref ([]: program)

(*
e.g.:
query   = a(Y, c(d, e))
program = a(b, c(X, e))
return = sub_q = Y -> b and sub_p = X -> d

or

query   = a(Y, c(d, e))
program = a(b, c(X, f))
return = exception Invalid_argument "conflict in atom or argument names."

or

query   = f(Y, c(d, e))
program = a
return = exception Invalid_argument "conflict for unification of an atom with a functor."

or

query   = a(b, c(d, e))
program = f(b, c(d, e))
return = exception Invalid_argument "conflict in functor or structure names."

or

query   = a(Y, c(d, e))
program = a(b)
return = exception Invalid_argument "conflict in functor or structure's arguments lengths."
*)
let rec most_general_unifier (sub: substitution) (query: term) (program: term) : substitution =
  (* note: "check", "occurs" and "delete" are impossible for prolog programs *)
  let sub' = ref(sub) in
  let query = Substitution.substitute sub' query in
  let program = Substitution.substitute sub' program in
  let sub = !sub' in
  match program with
    Variable(_, _, i_p) -> begin
      match query with
        Variable(_, str, i_q) ->
          let sub = sub |> Substitution.add i_q program in (* eliminate *)
          Hashtbl.add index2name i_q str;
          sub
      | Predicate(_) ->
          let sub = sub |> Substitution.add i_p query in (* eliminate *)
          sub
    end
  | Predicate({atom=(_, _, i_p); args=args_p}) -> begin
      match query with
        Variable(_, str, i_q) ->
          let sub = sub |> Substitution.add i_q program in (* eliminate *)
          Hashtbl.add index2name i_q str;
          sub
      | Predicate({atom=(_, _, i_q); args=args_q}) ->
          if i_p <> i_q then
            if (List.is_empty args_p) && (List.is_empty args_q) then
              raise (Invalid_argument "conflict in atom or argument names.") (* conflict *)
            else if (List.is_empty args_p) || (List.is_empty args_q) then
              raise (Invalid_argument "conflict for unification of an atom with a functor.") (* conflict *)
            else
              raise (Invalid_argument "conflict in functor or structure names.") (* conflict *)
          else
            try List.fold_left2 most_general_unifier sub args_q args_p (* decompose *)
            with (Invalid_argument _) ->
              raise (Invalid_argument "conflict in functor or structure's arguments lengths.") (* conflict *)
    end

let rec split_substitution ((sub_q: substitution), (sub_p: substitution), (sub: substitution ref)) (query: term) (program: term) : (substitution * substitution * substitution ref) =
  match program with
    Variable(_, _, i_p) -> begin
      match query with
        Variable(_, _, i_q) ->
          let sub_q = sub_q |> Substitution.add i_q (sub |> Substitution.find i_q) in
          begin try
            let sub_p = sub_p |> Substitution.add i_p (sub |> Substitution.find i_p) in
            (sub_q, sub_p, sub)
          with Not_found ->
            let sub_p = sub_p |> Substitution.add i_p query in
            (sub_q, sub_p, sub) end
      | Predicate(_) ->
          let sub_p = sub_p |> Substitution.add i_p (sub |> Substitution.find i_p) in
          (sub_q, sub_p, sub)
    end
  | Predicate({atom=_; args=args_p}) -> begin
      match query with
        Variable(_, _, i_q) ->
          let sub_q = sub_q |> Substitution.add i_q (sub |> Substitution.find i_q) in
          (sub_q, sub_p, sub)
      | Predicate({atom=_; args=args_q}) ->
          List.fold_left2 split_substitution (sub_q, sub_p, sub) args_q args_p
    end

(*
tries to unify the given term (in query) with the given clause (in program)
if successful, tries to recursively prove the goal in the clause
otherwise, returns false
*)
let rec sld_resolution (sub : substitution) (query: term) (cls: clause) : solution_set = 
  let pred, goal = begin
    match cls with
      Rule(pred, goal) -> pred, goal
    | Fact(pred) -> pred,  [[]] (* [[]] means _true *)
  end in
  let pred, goal = if is_simulated query then pred, goal else simulate_trm pred, simulate goal in
  try begin
    let sub_mgu = most_general_unifier sub query pred in
    let sub_out, sub_in, _ = split_substitution (empty_sub, empty_sub, ref(sub_mgu)) query pred in
    prove_query sub_in goal |>
      Seq.map (Substitution.sub_sub sub_out)
  end with Invalid_argument(_) -> _false

(*
to prove a term, one must be unifiable with at least one clause in the program
*)
and prove_term (term: term) (sub: substitution) : solution_set =
  List.to_seq !prog |>
    Seq.concat_map (sld_resolution sub term)

(*
to prove a conjunction, all of its terms must be satisfied
*)
and prove_conjunction (sub: substitution) (conj: conjunction) : solution_set =
  let fold f ls = List.fold_left f (Seq.return sub) ls in
  conj |>
    fold begin
      fun (acc: solution_set) (term: term) ->
        acc |>
          Seq.concat_map (prove_term term)
    end

(*
to prove a disjunction, at least one of its conjunctions must be satisfied
*)
and prove_disjunction (sub: substitution) (disj: disjunction) : solution_set =
  List.to_seq disj |>
    Seq.concat_map (prove_conjunction sub)

and prove_query (sub: substitution) (q: query) : solution_set =
  prove_disjunction sub q

let process_query (q: query) =
  let ls = prove_query empty_sub q in
  print_sub_list ls

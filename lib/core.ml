open Syntax
open Format
open Support.Pervasive

(* ------------------------   EVALUATION  ------------------------ *)

exception NoRuleApplies

module IntMap = Map.Make(Int)
type substitution = term IntMap.t (* de Bruijn index of Variables -> ground terms (Predicates) *)
(* true & false's index are assumed to be 1 & 0 *)
let empty_sub: substitution = IntMap.empty

type index_to_name = (int, string) Hashtbl.t (* de Bruijn index of Variables -> string representation of Variables *)
let index2name: index_to_name = Hashtbl.create 1024 (* used for printing *)

let prog = ref ([]: program)

type solution_set = substitution Seq.t
let _true: solution_set = Seq.return empty_sub (* meaning there is no need to substitute anything for goal to be satisfied i.e. there are no variables *)
and _false: solution_set = Seq.empty (* meaning there is no substitution that satisfies goal *)

(*
If X was substituted to Y and Y to a, substitution_find X will return a
*)
let rec substitution_find (key: int) (sub: substitution ref) : term = (* works like the find operation in disjoint set data structure *)
  let value = IntMap.find key !sub in
  match value with
    Predicate(_) -> value
  | Variable(_, _, i) ->
    try begin
      let anc = sub |> substitution_find i in
      sub := !sub |> IntMap.update key (Option.map (fun _ -> anc));
      anc
  end with Not_found -> value

let substitution_add (key: int) (value: term) (sub: substitution) : substitution = (* works like the union operation in disjoint set data structure *)
  IntMap.add key value sub

(*
uses a substitution to make term more ground. e.g.:
sub = X -> c
term = a(Y, b(X, d))
return = a(Y, b(c, d))
*)
let rec substitute (sub: substitution ref) (term: term) : term = match term with
    Variable(_, _, i) -> (try sub |> substitution_find i with Not_found -> term)
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

(*
e.g.:
term_q = a(Y, c(d, e))
term_c = a(b, c(X, e))
return = sub_out = Y -> b and sub_in = X -> d

or

term_q = a(Y, c(d, e))
term_c = a(b, c(X, f))
return = exception Invalid_argument "false"

or

term_q = a(Y, c(d, e))
term_c = f(b)
return = exception Invalid_argument "false"
*)
let rec most_general_unifier ((sub_out: substitution), (sub_in: substitution)) (term_q': term) (term_c': term) : (substitution * substitution) =
  let sub_out' = ref(sub_out) in
  let sub_in'  = ref(sub_in) in
  let term_q = substitute sub_out' term_q' in
  let term_c = substitute sub_in'  term_c' in
  let sub_out = !sub_out' in
  let sub_in  = !sub_in' in
  match term_c with
    Variable(_, _, i_c) -> begin
      match term_q with
        Variable(_, str, i_q) ->
          let sub_out = sub_out |> substitution_add i_q term_c in
          let sub_in = sub_in |> substitution_add i_c term_q in
          Hashtbl.add index2name i_q str;
          (sub_out, sub_in)
      | Predicate(_) ->
          let sub_in = sub_in |> substitution_add i_c term_q in
          (sub_out, sub_in)
    end
  | Predicate({atom=(_, _, i_c); args}) -> begin
      match term_q with
        Variable(_, str, i_q) ->
          let sub_out = sub_out |> substitution_add i_q term_c in
          Hashtbl.add index2name i_q str;
          (sub_out, sub_in)
      | Predicate({atom=(_, _, i_q); args=args_q}) ->
          if i_c <> i_q then raise (Invalid_argument "false") else
            List.fold_left2 most_general_unifier (sub_out, sub_in) args_q args
    end

(*
tries to unify the given term (in query) with the given clause (in program)
if successful, tries to recursively prove the goal in the clause
otherwise, returns false
*)
let rec sld_resolution (sub : substitution) (trm_q: term) (cls: clause) : solution_set = 
  let pred_c, goal = begin
    match cls with
      Rule(pred, goal) -> pred, goal
    | Fact(pred) -> pred,  [[]] (* [[]] means _true *)
  end in try begin
    let sub_out, sub_in = most_general_unifier (sub, empty_sub) trm_q pred_c in
    prove_query sub_in goal |>
      Seq.map (sub_sub sub_out)
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

(* ---------------------------------------------------------------------- *)
(* -----------------------------   Printing  ---------------------------- *)
(* ---------------------------------------------------------------------- *)

let rec print_ground_term: term -> unit = function
    Variable(_, str, _) -> pr str
  | Predicate({atom=(_, str, _); args=[]}) -> pr str
  | Predicate({atom=(_, str, _); args}) ->
      pr str;
      pr "(";
      let rec print_args = function
          [] -> ()
        | term::[] -> obox (); print_ground_term term; cbox ()
        | term::ls -> obox (); print_ground_term term; cbox (); pr ", "; print_args ls
      in
      print_args args;
      pr ")"

let rec print_sub: (int*term) list -> unit = function
    [] -> ()
  | (i, tm)::[] ->
    pr (Hashtbl.find index2name i);
    pr " = ";
    obox ();
    print_ground_term tm;
    cbox ();
    pr " "
  | (i, tm)::ls -> 
    pr (Hashtbl.find index2name i);
    pr " = ";
    obox ();
    print_ground_term tm;
    cbox ();
    pr ",";
    print_space();
    print_sub ls

let read_char () =
  let termio = Unix.tcgetattr Unix.stdin in
  let () =
      Unix.tcsetattr Unix.stdin Unix.TCSADRAIN
          { termio with Unix.c_icanon = false } in
  let stdin_channel = Unix.in_channel_of_descr Unix.stdin in
  let res = input_char stdin_channel in
  Unix.tcsetattr Unix.stdin Unix.TCSADRAIN termio;
  res


let rec print_sub_list (ls': solution_set) = match ls'() with
    Seq.Nil -> ()
  | Seq.Cons(sub, ls) when Seq.is_empty ls ->
      print_sub @@ IntMap.to_list sub;
      pr ".";
      print_newline()
  | Seq.Cons(sub, ls) ->
      print_sub @@ IntMap.to_list sub;
      print_flush();
      match read_char() with
          ';' ->
            print_newline();
            print_sub_list ls
        | _   -> 
            print_endline "\b."

let print_sub_list (ls: solution_set) = match ls() with
    Seq.Nil -> print_endline "false ."
  | Seq.Cons(x, _) when x = empty_sub -> print_endline "true ."
  | _ -> print_sub_list ls

let process_query (db: program) (q: query) =
  prog := db;
  let ls = prove_query empty_sub q in
  print_sub_list ls

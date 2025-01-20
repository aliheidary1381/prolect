open Syntax
open Format
open Unix
open Support.Pervasive
open Support.Error

(* ------------------------   EVALUATION  ------------------------ *)

exception NoRuleApplies

module IntMap = Map.Make(Int)
type substitution = term IntMap.t (* de Bruijn index of Variables -> ground terms (Predicates) *)
(* true & false's index are assumed to be 1 & 0 *)
let empty_sub: substitution = IntMap.empty

type index_to_name = (int, string) Hashtbl.t (* de Bruijn index of Variables -> string representation of Variables *)
let index2name: index_to_name = Hashtbl.create 1024 (* used for printing *)

let prog = ref ([]: program)

type substitution_list = substitution Seq.t

let rec substitution_find key sub = (* works like the find operation in disjoint set data structure *)
  let value = IntMap.find key sub
    in match value with
    | Predicate(_) -> (value, sub)
    | Variable(_, _, i) -> (
      try (
        let anc, sub = sub |> substitution_find i in
        let sub = sub |> IntMap.update key (Option.map (fun _ -> anc)) in
        (anc, sub)
      ) with Not_found -> (value, sub)
    )

let substitution_add key value sub = (* works like the union operation in disjoint set data structure *)
  IntMap.add key value sub

let rec substitute sub term = match term with
    Variable(_, _, i) -> (try sub |> substitution_find i with Not_found -> (term, sub))
  | Predicate({atom; args}) -> let args, sub = sub_args sub args in Predicate({atom; args}), sub

and sub_args sub' args = 
  let sub = ref sub' in
  List.map (fun trm -> let trm', s = substitute !sub trm in sub := s; trm') args, !sub

let sub_sub sub_out sub_in' =
  let sub_in = ref sub_in' in
  sub_out |> IntMap.map (fun trm_q -> let term, s = substitute !sub_in trm_q in sub_in := s; term)

let rec unify_terms sub_out' sub_in' goal term_q' term_c' =
  let term_q, sub_out = substitute sub_out' term_q' in
  let term_c, sub_in = substitute sub_in'  term_c' in
  match term_c with
  | Variable(_, _, i_c) -> (match term_q with
    | Variable(_, str, i_q) ->
        let sub_out = sub_out |> substitution_add i_q term_c in
        let sub_in = sub_in |> substitution_add i_c term_q in
        Hashtbl.add index2name i_q str;
        (sub_out, sub_in, goal)
    | Predicate(_) ->
        let sub_in = sub_in |> substitution_add i_c term_q in
        (sub_out, sub_in, goal)
  )
  | Predicate({atom=(_, _, i_c); args}) -> (match term_q with
    | Variable(_, str, i_q) ->
        let sub_out = sub_out |> substitution_add i_q term_c in
        Hashtbl.add index2name i_q str;
        (sub_out, sub_in, goal)
    | Predicate({atom=(_, _, i_q); args=args_q}) ->
        if i_c <> i_q then (sub_out, sub_in, []) else unify_argss sub_out sub_in goal args_q args
  )

and unify_argss sub_out sub_in goal args_q args_c = match args_c with
  | [] -> (match args_q with
      [] -> (sub_out, sub_in, goal) 
    | _::_ -> (sub_out, sub_in, [])
  )
  | trm_c::ls -> (match args_q with
      [] -> (sub_out, sub_in, [])
    | trm_q::ls_q ->
        let (sub_out, sub_in, goal) = unify_terms sub_out sub_in goal trm_q trm_c in
        unify_argss sub_out sub_in goal ls_q ls
  )

let unify_term_clause sub trm_q = function
  | Rule(pred_c, goal) -> unify_terms sub empty_sub goal trm_q pred_c
  | Fact(pred_c) -> unify_terms sub empty_sub [[]] trm_q pred_c

let rec mgu = function (sub_out, sub_in, goal) ->
  most_general_unifier sub_in goal |> Seq.map (sub_sub sub_out)

and dfs_on_term sub term =
  (List.to_seq !prog) |> Seq.map (unify_term_clause sub term) |> Seq.concat_map mgu

and dfs_on_conj conj sub = match conj with
  | [] -> Seq.return sub
  | tm::ls ->
      let sub_list = dfs_on_term sub tm in
      Seq.concat_map (dfs_on_conj ls) sub_list

and dfs_on_disj sub = function
  | [] -> Seq.empty
  | conj::ls ->
    Seq.append (dfs_on_conj conj sub) (dfs_on_disj sub ls)

and most_general_unifier sub q =
  dfs_on_disj sub q

(* ---------------------------------------------------------------------- *)
(* Printing *)

let rec print_ground_term = function
  | Variable(_) -> print_endline "error"
  | Predicate({atom=(_, str, _); args=[]}) -> pr str
  | Predicate({atom=(_, str, _); args}) ->
      let rec iter_args = function
        | [] -> ()
        | term::[] -> obox (); print_ground_term term; cbox ()
        | term::ls -> obox (); print_ground_term term; cbox (); pr ", "
      in
      pr str;
      pr "(";
      iter_args args;
      pr ")"

let rec iter_sub = function
  | [] -> ()
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
    iter_sub ls

let read_char () =
  let termio = Unix.tcgetattr Unix.stdin in
  let () =
      Unix.tcsetattr Unix.stdin Unix.TCSADRAIN
          { termio with Unix.c_icanon = false } in
  let stdin_channel = Unix.in_channel_of_descr Unix.stdin in
  let res = input_char stdin_channel in
  Unix.tcsetattr Unix.stdin Unix.TCSADRAIN termio;
  res


let rec iter_sub_list f ls' = match Seq.uncons ls' with
  | None -> ()
  | Some(sub, ls) when Seq.is_empty ls ->
      f @@ IntMap.to_list sub;
      pr ".";
      print_newline();
      print_endline ""
  | Some(sub, ls) ->
      f @@ IntMap.to_list sub;
      print_flush();
      match read_char() with
        | ';' ->
            print_newline();
            iter_sub_list f ls
        | _   -> 
            print_endline "\b.";
            print_endline ""

let print_sub_list ls = match Seq.uncons ls with
  | None -> print_endline "false ."
  | Some(x, ls') when x = empty_sub -> print_endline "true ."
  | _ -> iter_sub_list iter_sub ls

let process_query db q =
  prog := db;
  let ls = most_general_unifier empty_sub q in
  print_sub_list ls

open Format
open Support.Pervasive
open Syntax

type index_to_name = (int, string) Hashtbl.t (* Barendregt index of Variables -> string representation of Variables *)
let index2name: index_to_name = Hashtbl.create 1024 (* used for printing *)


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

[%%if os_type <> "Win32"]
let read_char () =
  let termio = Unix.tcgetattr Unix.stdin in
  let () =
      Unix.tcsetattr Unix.stdin Unix.TCSADRAIN
          { termio with Unix.c_icanon = false } in
  let stdin_channel = Unix.in_channel_of_descr Unix.stdin in
  let res = input_char stdin_channel in
  Unix.tcsetattr Unix.stdin Unix.TCSADRAIN termio;
  res
[%%else]
let read_char () =
  input_char stdin
[%%endif]

let rec print_sub_list (sub': substitution) (ls': solution_set) =
  print_sub @@ IntMap.to_list sub'; print_flush();
  match ls'() with
    Seq.Nil ->
      pr ".";
      print_newline()
  | Seq.Cons(sub, ls) ->
      match read_char() with
          ';' ->
            print_newline();
            print_sub_list sub ls
        | _   -> 
            print_endline "\b."

let print_sub_list (ls: solution_set) = match ls() with
    Seq.Nil -> print_endline "false ."
  | Seq.Cons(x, _) when x = empty_sub -> print_endline "true ."
  | Seq.Cons(x, ls') -> print_sub_list x ls'

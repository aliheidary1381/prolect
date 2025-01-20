(* Module Main: The main program.  Deals with processing the command
   line, reading files, building and connecting lexers and parsers, etc.
*)

open Format
open String
open Prolect.Support.Error
open Prolect.Syntax
open Prolect.Core

module Parser = Prolect.Parser
module Lexer = Prolect.Lexer

let searchpath = ref [""]

let argDefs = [
  "-I",
      Arg.String (fun f -> searchpath := f::!searchpath),
      "Append a directory to the search path"]

let parseArgs () =
  let inFile = ref (None : string option) in
  Arg.parse argDefs
     (fun s ->
       match !inFile with
         Some(_) -> err "You must specify exactly one input file"
       | None -> inFile := Some(s))
     "";
  match !inFile with
      None -> err "You must specify an input file"
    | Some(s) -> s

let openfile infile = 
  let rec trynext l = match l with
        [] -> err ("Could not find " ^ infile)
      | (d::rest) -> 
          let name = if d = "" then infile else (d ^ "/" ^ infile) in
          try open_in name
            with Sys_error _ -> trynext rest
  in trynext !searchpath

let parseFile inFile =
  let pi = openfile inFile
  in let lexbuf = Lexer.create inFile pi
  in let result =
    try Parser.file Lexer.main lexbuf with Parsing.Parse_error -> 
    error (Lexer.info lexbuf) "Parse error"
in
  Parsing.clear_parser(); close_in pi; result

let rec read_til_dot ?(prompt = "?- ") () = 
  print_string prompt;
  print_flush();
  let line = read_line() in
    if contains line '.' then
      line
    else
      line ^ (read_til_dot ~prompt: "|    " ())

let parseString str =
  let lexbuf = Lexer.createFromStr str
  in let result =
    try Parser.query Lexer.main lexbuf with Parsing.Parse_error -> 
      print_endline "Parse Error"; print_flush(); []
in
  Parsing.clear_parser(); result
 
let alreadyImported = ref ([] : string list)
let db = ref ([]: program)

let process_file f  =   
  if not @@ List.mem f (!alreadyImported) then
    alreadyImported := f :: !alreadyImported;
    db := (parseFile f) @ !db

let process_db db q =  
  open_hvbox 0;
  process_query db q;
  force_newline();
  print_flush()

let rec toplevel f =
  try (
    let text = "?- " ^ read_til_dot() in
    let q = parseString text in
      process_db !db q;
      toplevel ()
  ) with End_of_file ->
    print_endline "";
    print_endline "exit"; 0


let main () = 
  let inFile = parseArgs() in
  process_file inFile;
  print_endline "Welcome to Prolect (version 1.0.0)";
  print_endline "";
  toplevel ()

let () = set_max_boxes 1000
let () = set_margin 67
let res = 
  Printexc.print (fun () -> 
    try main()
    with Exit x -> x)
  ()
let () = print_flush()
let () = exit res

(* Module Main: The main program.  Deals with processing the command
   line, reading files, building and connecting lexers and parsers, etc.
*)

open Format
open String
open Prolect.Support.Error
open Prolect.Core

module Parser = Prolect.Parser
module Lexer = Prolect.Lexer

(* let _ = Printexc.record_backtrace true *)

let searchpath = ref [""]

let argDefs = [
  "-I",
      Arg.String (fun f -> searchpath := f::!searchpath),
      "Append a directory to the search path"]

let openfile (infile: string) = 
  let rec trynext l = match l with
        [] -> err ("Could not find " ^ infile)
      | (d::rest) -> 
          let name = if d = "" then infile else (d ^ "/" ^ infile) in
          try open_in name
            with Sys_error _ -> trynext rest
  in trynext !searchpath

let parseFile (inFile: string) =
  let pi = openfile inFile
  in let lexbuf = Lexer.create inFile pi
  in let result =
    try Parser.file Lexer.main lexbuf with Parsing.Parse_error -> 
    error (Lexer.info lexbuf) "Parse error"
in
  Parsing.clear_parser(); close_in pi; result

let rec read_til_dot ?(prompt = "?- ") () = 
  let line =
    match LNoise.linenoise prompt with
      None -> ""
    | Some l -> l in
  if contains line '.' then
    line
  else
    line ^ " " ^ (read_til_dot ~prompt: "|    " ())

let parseString (str: string) =
  let lexbuf = Lexer.createFromStr str
  in let result =
    try Parser.query Lexer.main lexbuf with Parsing.Parse_error -> 
      print_endline "Parse Error"; print_flush(); []
in
  Parsing.clear_parser(); result
 
let alreadyImported = ref ([] : string list)

let process_file (f: string)  =   
  if not @@ List.mem f (!alreadyImported) then
    alreadyImported := f :: !alreadyImported;
    let db = parseFile f in
    prog := db @ !prog

let rec toplevel () =
  try (
    let text = read_til_dot() in
    LNoise.history_add text |> ignore;
    LNoise.history_save ~filename:"history.txt" |> ignore;
    let q = parseString ("?- " ^ text) in
    open_hvbox 0;
    process_query q;
    force_newline();
    print_flush();
      toplevel ()
  ) with Stdlib.Sys.Break -> 0


let main () = 
  let inFiles = ref ([] : string list) in
  Arg.parse argDefs (fun s -> inFiles := s::!inFiles) 
{|prolect (Prolect) 1.1.2
Usage: prolect files...|};
  List.iter process_file !inFiles;
  print_endline "Welcome to Prolect (version 1.1.2)";
  print_endline "";
  LNoise.set_multiline true;
  LNoise.history_load ~filename:"history.txt" |> ignore;
  LNoise.history_set ~max_length:100 |> ignore;
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

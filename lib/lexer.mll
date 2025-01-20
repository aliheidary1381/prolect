(* 
   The lexical analyzer: lexer.ml is generated automatically
   from lexer.mll.
   
   The only modification commonly needed here is adding new keywords to the 
   list of reserved words at the top.  
*)

{
open Support.Error

let reservedWords = [
  (* No keywords *)
  
  (* Symbols *)
  (".", fun i -> Parser.ENDCLAUSE i);
  (";", fun i -> Parser.DISJ i);
  (",", fun i -> Parser.COMMA i);
  ("(", fun i -> Parser.LPAREN i); 
  (")", fun i -> Parser.RPAREN i);

  (* Special compound symbols: *)
  (":-", fun i -> Parser.RULE i);
  ("?-", fun i -> Parser.QUERY i);
]

(* Support functions *)

type buildfun = info -> Parser.token
let (symbolTable : (string,buildfun) Hashtbl.t) = Hashtbl.create 1024
let _ =
  List.iter (fun (str,f) -> Hashtbl.add symbolTable str f) reservedWords

let createAtomID i str = Parser.LCID {i=i;v=str}

let createVariableID i str = Parser.UCID {i=i;v=str}

let createReservedID i str = (Hashtbl.find symbolTable str) i

let lineno   = ref 1
and depth    = ref 0
and start    = ref 0

and filename = ref ""
let startLex = ref (FI("", 0, 0))

let createFromStr str =
  lineno := 1; start := 0; Lexing.from_string str

let create inFile stream =
  if not (Filename.is_implicit inFile) then filename := inFile
  else filename := Filename.concat (Sys.getcwd()) inFile;
  lineno := 1; start := 0; Lexing.from_channel stream

let newline lexbuf = incr lineno; start := (Lexing.lexeme_start lexbuf)

let info lexbuf =
  createInfo (!filename) (!lineno) (Lexing.lexeme_start lexbuf - !start)
}


(* Character classes *)
let upper_case = ['A'-'Z']
let underline = '_'
let lower_case = ['a'-'z']
let digit = ['0'-'9']
let alphanumerical = upper_case | underline | lower_case | digit
let blank_space = [' ' '\t' '\009' '\012']
let end_of_line = "\r\n" | '\n' | '\r'
let open_line_comment = '%'
let open_block_comment = "/*"
let close_block_comment = "*/"

(* Atoms & Variables *)
let atom = lower_case alphanumerical * (* includes true & false *)
let variable = (upper_case | underline) alphanumerical *
let reserved = ['(' ')' '.' ';' ','] | ":-" | "?-"
let line_comment = open_line_comment [^ '\n'] *

(* The main body of the lexical analyzer *)

rule main = parse
  blank_space+     { main lexbuf }

| blank_space* end_of_line { newline lexbuf; main lexbuf }

| line_comment { main lexbuf }

| close_block_comment { error (info lexbuf) "Unmatched end of block comment" }

| open_block_comment { depth := 1; startLex := info lexbuf; block_comment lexbuf; main lexbuf }

| reserved as token { createReservedID (info lexbuf) token }

| atom as token { createAtomID (info lexbuf) token }

| variable as token { createVariableID (info lexbuf) token }

| eof { Parser.EOF(info lexbuf) }

| _  { error (info lexbuf) "Illegal character" }

and block_comment = parse
  "/*"
    { depth := succ !depth; block_comment lexbuf }
| "*/"
    { depth := pred !depth; if !depth > 0 then block_comment lexbuf }
| eof
    { error (!startLex) "Block comment not terminated" }
| [^ '\n']
    { block_comment lexbuf }
| "\n"
    { newline lexbuf; block_comment lexbuf }

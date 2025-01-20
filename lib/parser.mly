/*  
 *  Yacc grammar for the parser.  The files parser.mli and parser.ml
 *  are generated automatically from parser.mly.
 */

%{
open Support.Error
open Syntax

(* Context management *)
let first_free_index = ref 2 (* 0 & 1 are reserved for false & true *)

type mutable_index = (string, int) Hashtbl.t
let atoms_index: mutable_index = Hashtbl.create 1024
let _ = Hashtbl.add atoms_index "false" 0
let _ = Hashtbl.add atoms_index "true"  1

module SMap = Map.Make(String)
type immutable_index = int SMap.t

let pick_atom_index str =
  try Hashtbl.find atoms_index str
  with Not_found ->
    let i = !first_free_index in
    Hashtbl.add atoms_index str i;
    first_free_index := i + 1;
    i

let pick_variable_index variables_index str =
  try
    let i = variables_index |> SMap.find str in
      i, variables_index
  with Not_found ->
    let i = !first_free_index in
    let variables_index = variables_index |> SMap.add str i in
    first_free_index := i + 1;
    i, variables_index
%}

/* ---------------------------------------------------------------------- */
/* Preliminaries */

/* We first list all the tokens mentioned in the parsing rules
   below.  The names of the tokens are common to the parser and the
   generated lexical analyzer.  Each token is annotated with the type
   of data that it carries; normally, this is just file information
   (which is used by the parser to annotate the abstract syntax trees
   that it constructs), but sometimes -- in the case of variables and
   atoms -- more information is provided.
 */

/* No keywords or constant value tokens */

/* Identifier tokens */
%token <string Support.Error.withinfo> UCID  /* uppercase-initial */
%token <string Support.Error.withinfo> LCID  /* lowercase/symbolic-initial (includes true & false) */

/* Symbolic tokens:         ,     .      eof (      )      ;    :-   ?-   */
%token <Support.Error.info> COMMA ENDCLAUSE  EOF LPAREN RPAREN DISJ RULE QUERY

%left ENDCLAUSE  EOF
%nonassoc RULE QUERY
%nonassoc LPAREN RPAREN
%left DISJ
%left COMMA
%nonassoc UCID LCID

/* ---------------------------------------------------------------------- */
/* The starting productions of the generated parser are the syntactic classes
   file & query.  The type that is returned when a file or query is recognized is
     Syntax.file or Syntax.query.
   
*/

%start file query
%type < Syntax.program > file
%type < Syntax.program > program
%type < Syntax.clause  > clause
%type < Syntax.fact    > fact
%type < Syntax.rule    > rule
%type < Syntax.query   > query
%type < immutable_index -> (Syntax.goal        * immutable_index) > goal
%type < immutable_index -> (Syntax.disjunction * immutable_index) > disjunction
%type < immutable_index -> (Syntax.conjunction * immutable_index) > conjunction
%type < immutable_index -> (Syntax.predicate   * immutable_index) > predicate
%type < immutable_index -> (Syntax.args        * immutable_index) > args
%type < immutable_index -> (Syntax.term        * immutable_index) > term
%type < immutable_index -> (Syntax.term        * immutable_index) > variable

%%
/* ---------------------------------------------------------------------- */
/* Main body of the parser definition */

/* The top level of a file is a sequence of clauses, each terminated
   by a dot. */

file:
    program EOF
      { let tl = $1 in tl }

program: // clause list
    clause
      { let cl = $1 in
          [cl] }
  | clause program
      { let cl = $1 in
          let ls = $2 in
            cl::ls }

clause:
    fact
      { let f = $1 in
            Fact f }
  | rule
      { let r = $1 in
            Rule r }

fact:
    predicate ENDCLAUSE 
      { let variables_index: immutable_index = SMap.empty in
          let (pred, _) = $1 variables_index in
            Predicate(pred) }

rule:
    predicate RULE goal ENDCLAUSE 
      { let variables_index: immutable_index = SMap.empty in
          let (p, variables_index) = $1 variables_index in
            let (g, _) = $3 variables_index in
              (Predicate(p), g) }

query:
    QUERY goal ENDCLAUSE 
      { let variables_index: immutable_index = SMap.empty in
          let (g, _) = $2 variables_index in
            g }

goal:
    disjunction
      { $1 }

disjunction: // conjunction list
    conjunction DISJ disjunction
      { fun ctx -> 
          let conj, _ = $1 ctx in
            let ls, _ = $3 ctx in
              conj::ls, ctx }
  | conjunction
      { fun ctx -> 
          let conj, _ = $1 ctx in
            [conj], ctx }

conjunction: // term list
    term COMMA conjunction
      { fun ctx -> 
          let tm, ctx = $1 ctx in
            let ls, ctx = $3 ctx in
              tm::ls, ctx }
  | term
      { fun ctx -> 
          let tm, ctx = $1 ctx in
            [tm], ctx }

predicate:
    atom LPAREN args RPAREN
      { fun ctx ->
          let atm = $1 in
            let ags, ctx = $3 ctx in
              ({ atom = atm; args = ags }, ctx) }
  | atom
      { fun ctx ->
          let atm = $1 in
            ({ atom = atm; args = [] }, ctx) }

args:
    term COMMA args
      { fun ctx ->
          let trm, ctx = $1 ctx in
            let ls, ctx = $3 ctx in
              trm::ls, ctx }
  | term
      { fun ctx ->
          let trm, ctx = $1 ctx in
            [trm], ctx }

term:
    predicate
      { fun ctx ->
          let prd, ctx = $1 ctx in
            (Predicate prd), ctx }
  | variable
      { $1 }

atom:
    LCID
      { let atm = $1 in
          (atm.i, atm.v, pick_atom_index atm.v) }

variable:
    UCID
      { fun ctx -> 
          let var = $1 in
            let i, ctx = pick_variable_index ctx var.v in
              Variable (var.i, var.v, i), ctx }

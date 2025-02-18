# Prolect
An OCaml Derivation of the [Warren Abstract Machine](https://en.wikipedia.org/wiki/Warren_Abstract_Machine).

Language support is limited to pure prolog i.e. predicate logic with Horn clauses.

This is a very simple & plain prolog interpreter with no support for negation as failure,
integers, lists, operators or built-in predicates.

Implementaion-wise, there is no tabling or memoization nor any type of term indexing.
The core part of the interpreter is only ~100 LOC.
## Usage
```
$ dune build . --profile release
```

Run the REPL
```
$ dune exec prolect test.pl --profile release
Welcome to Prolect (version 1.0.0)

?- ancestor(terah, jacob).
true .

?- ancestor(terah, X).
X = sarah ;
X = abraham ;
X = isaac .

?- ^C
$
```

## Future plans
* Improve error handling
* Add files importing facilities
* Add term indexing
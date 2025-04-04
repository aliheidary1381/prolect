# Prolect

An OCaml Derivation of the [Warren Abstract Machine](https://en.wikipedia.org/wiki/Warren_Abstract_Machine).

Language support is limited to pure prolog i.e. predicate logic with Horn clauses.

This is a very simple & plain prolog interpreter with no support for negation as failure,
integers, lists, operators or built-in predicates.

Implementaion-wise, there is no tabling or memoization nor any type of term indexing.
The core part of the interpreter is only ~100 LOC.

## Usage

```shell
$ dune build . --profile release
```

Run the REPL

```shell
$ dune exec prolect test.pl --profile release

```

```prolog
Welcome to Prolect (version 1.1.1)

?- ancestor(terah, jacob).
true .

?- ancestor(terah, X).
X = sarah ;
X = abraham ;
X = isaac ;
X = jacob ;
X = isaac ;
X = ismael ;
X = jacob .

?- loves(sarah, X).
X = isaac ;
X = loves(sarah, isaac) ;
X = loves(sarah, loves(sarah, isaac)) .

?- ^C
```

```shell
$
```

## Future plans

* Improve error handling
* Add files importing facilities
* Add term indexing

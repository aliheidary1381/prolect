# Prolect

An OCaml Derivation of the [Warren Abstract Machine](https://en.wikipedia.org/wiki/Warren_Abstract_Machine).

Language support is limited to pure Prolog i.e. predicate logic with Horn clauses.

This is a very simple & plain Prolog interpreter with no support for negation as failure,
integers, lists, operators or built-in predicates.

Implementaion-wise, there is no tabling or memoization nor any type of term indexing.
The core part of the interpreter is only ~100 LOC.

## Usage

### Linux

```shell
./prolect.exe
```

### Other OSes

First, install the OCaml Runtime. Then:

```shell
ocamlrun prolect.bc
```

### Compile it yourself

If you don't want to use the bytecode interpreter, you can compile the project yourself
(e.g. for getting a performance boost).

First, install the dependencies (`ocaml`, `dune`, etc.). Then:

```shell
dune exec prolect --profile release
```

#### The exe

To get the executable:

```shell
dune build . --profile release
```

Run the REPL:

```shell
_build/default/bin/prolect.exe
```

## Example

Provided that you've given the test consulting file
(i.e. running `prolect test.pl` instead of `prolect`):

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

## Future plans

* Improve error handling
* Add files importing facilities
* Add term indexing

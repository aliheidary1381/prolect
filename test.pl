/* Examples for testing */

male(terah). 
male(abraham).
male(nahor). 
male(haran).
male(isaac). 
male(ismael).
male(jacob).

female(sarah). 
female(hagar). 

father(terah, sarah). 
father(terah, abraham). 
father(abraham, isaac). 
father(isaac, jacob). 
father(abraham, ismael). 


mother(sarah, isaac).
mother(hagar, ismael).

parent(Parent, Child) :- father(Parent, Child).
parent(Parent, Child) :- mother(Parent, Child).

son_of(Child, Parent) :-
  male(Child), parent(Parent, Child).

daughter_of(Child, Parent) :-
  female(Child), parent(Parent, Child).


ancestor(Ancestor, Descendant) :- 
  parent(Ancestor, Descendant).
ancestor(Ancestor, Descendant) :-
  parent(Ancestor, X), ancestor(X, Descendant).

loves(Person1, Person2) :- mother(Person1, Person2).
loves(Person1, loves(Person1, Person2)) :- loves(Person1, Person2).

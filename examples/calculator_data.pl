:- dynamic([current/1, memory/1, operation/1, maxlength/1]).
:- initialization(init).

init :-
  asserta(current("0")),
  asserta(memory("0")),
  asserta(operation(0)),
  asserta(maxlength(30)).

currentx(X) :-
    clause(current(X), true),
    !.
currentx("0").

current_length(L) :-
    currentx(X),
    length(X, L).

set_current(X) :-
    retractall(current(_)),
    asserta(current(X)).

add_current(X) :-
    retract(current(C)),
    append(C, X, Y),
    asserta(current(Y)),
    !.

maxlengthx(X) :-
    clause(maxlength(X), true).


memoryx(X) :-
    clause(memory(X), true).

set_memory(X) :-
    retractall(memory(_)),
    asserta(memory(X)).

operationx(X) :-
    clause(operation(X), true).

set_operation(X) :-
    retractall(operation(_)),
    asserta(operation(X)).

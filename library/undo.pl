:- module(undo, [undoable_update/2, undo_update/2, undoable_assert/1, undoable_retract/1, undoable_term/2]).

:- meta_predicate((
    undoable_update((:), (:)),
    undoable_assert((:)),
    undoable_retract((:)),
    undo_update((:), (:))
    )).

:- dynamic(undoable_term/2).

undoable_update(M1: VarG, G2) :-
    var(VarG),
    !,
    undoable_update1(M1: VarG, G2).
undoable_update(_M1: M2: G, G2) :-
    !,
    undoable_update(M2 : G, G2).
undoable_update(Old, New) :-
    undoable_update1(Old, New),
    !.

undoable_update1(G1, M1 :VarG2) :-
    var(VarG2),
    !,
    undoable_update2(G1, M1 :VarG2).
undoable_update1(G1, _M1 : M2 : G2) :-
    !,
    undoable_update1(G1, M2 : G2).
undoable_update1(Old, New) :-
    undoable_update2(Old, New),
    !.

undoable_update2(OldTerm, NewTerm) :-
    retract(OldTerm)
      -> asserta(NewTerm),
         (OldTerm \= NewTerm
          -> asserta(undoable_term(OldTerm, NewTerm))
         ;
         true
         )
    ;
    undoable_template(OldTerm, Template),
    \+ call(Template)
      -> asserta(NewTerm),
         asserta(undoable_term('$none', NewTerm))
    ;
    throw(invalid_update(OldTerm, NewTerm)).

undoable_template(M:OldTerm, M:Template) :-
    !,
    functor(OldTerm, Functor, Arity),
    functor(Template, Functor, Arity),
    arg(1, OldTerm, ID),
    arg(1, Template, ID).
undoable_template(OldTerm, Template) :-
    functor(OldTerm, Functor, Arity),
    functor(Template, Functor, Arity),
    arg(1, OldTerm, ID),
    arg(1, Template, ID).

undoable_retract(M1 : VarTerm) :-
    var(VarTerm),
    !,
    throw(invalid_undoable_retract(M1 : VarTerm)).
undoable_retract(_M1 : M2 : Term) :-
    !,
    undoable_retract(M2 : Term).
undoable_retract(Term) :-
    undoable_retract1(Term).

undoable_retract1(Term) :-
    (retract(Term)
      -> asserta(undoable_term(Term, '$none'))
    ;
    true
    ),
    !.


undoable_assert(M1: VarG) :-
    var(VarG),
    !,
    throw(invalid_undoable_assert(M1: VarG)).
undoable_assert(_M1: M2: G) :-
    !,
    undoable_assert(M2 : G).
undoable_assert(Term) :-
    undoable_assert1(Term),
    !.

undoable_assert1(Term) :-
    undoable_template(Term, Template),
    \+ call(Template)
      -> asserta(Term),
         asserta(undoable_term('$none', Term))
    ;
    throw(invalid_asserta(Term)).

undo_update(M1: VarG, G2) :-
    var(VarG),
    !,
    undo_update1(M1 : VarG, G2).
undo_update(_M1: M2: G, G2) :-
    !,
    undo_update(M2 : G, G2).
undo_update(Old, New) :-
    undo_update1(Old, New),
    !.

undo_update1(G1, M1 :VarG2) :-
    var(VarG2),
    !,
    undo_update2(G1, M1 : VarG2).
undo_update1(G1, _M1 : M2 : G2) :-
    !,
    undo_update1(G1, M2 : G2).
undo_update1(G1, G2) :-
    !,
    undo_update2(G1, G2).

% undo_update1(OldTerm, NewTerm)
undo_update2(OldTerm, NewTerm) :-
    undoable_term(OldTerm, NewTerm)
      -> repeat,
         retract(undoable_term(OldTermX, NewTermX)),
         undo_single_update(OldTermX, NewTermX),
         OldTermX = OldTerm,
         NewTermX = NewTerm
    ;
    throw(undo_not_defined(OldTerm, NewTerm)).

% undo_single_update(OldTermX, NewTermX)
% executes a single retract/asserta.
% The cut (!) is needed to prevent retract(NewTermX)
% from endlessly succeeding on redo when OldTermX=NewTermX.

undo_single_update('$none', NewTermX) :-
    required_retract(NewTermX),
    !.
undo_single_update(OldTermX, '$none') :-
    asserta(OldTermX),
    !.
undo_single_update(OldTermX, NewTermX) :-
    required_retract(NewTermX),
    asserta(OldTermX),
    !.

required_retract(NewTermX) :-
    retract(NewTermX)
      -> true
    ;
    throw(missing_term_for_undo(NewTermX)).

%   File   : NOT.PL
%   Author : R.A.O'Keefe
%   Updated: 17 November 1983
%   Purpose: "suspicious" negation 

/*  This file defines a version of 'not' which checks that there are
    no free variables in the goal it is given to "disprove".  Bound
    variables introduced by the existential quantifier ^ or set/bag
    dummy variables are accepted.  If any free variables are found, 
    a message is printed on the terminal and a break level entered.

    It is intended purely as a debugging aid, though it shouldn't slow
    interpreted code down much.  There are several other debugging
    aids that you might want to use as well, particularly
	unknown(_, trace)
    which will detect calls to undefined predicates (as opposed to
    predicates which have clauses that don't happen to match).

    The predicate free_variables/4 defined in this files is also used
    by the set_of/bag_of code.

    Note: in Dec-10 Prolog you should normally use "\+ Goal" instead
    of "not(Goal)".  In C-Prolog you can use either, and would have to
    do some surgery on pl/init to install this version of "not".  The
    reason that I have called this predicate "not" is so that people
    can choose whether to use the library predicate not/1 (in Invoca.Pl)
    or this debugging one, not because I like the name.
*/
/*
:- public
	(not)/1.		%   new checking denial

:- mode
	explicit_binding(+,+,-,-),
	free_variables(+,+,+,-),
	    free_variables(+,+,+,+,-),
	list_is_free_of(+,+),
	not(+),
	term_is_free_of(+,+),
	    term_is_free_of(+,+,+).
*/

:- module(not, [free_variables/4, free_variables/5, not/1 ]).

'\\=='(A,B) :-
    'bootstrap_js:\\=='(A,B).

not(Goal) :-
	free_variables(Goal, [], [], Vars),
	Vars \== [], !,
	telling(Old), tell(user),
	nl, write('** '), write(not(Goal)),
	nl, write('-- free variables '), write(Vars),
	nl, break,
	tell(Old), !,
	call(Goal),
	!, fail.
not(Goal) :-
	call(Goal),
	!, fail.
not(_).


%   In order to handle variables properly, we have to find all the 
%   universally quantified variables in the Generator.  All variables
%   as yet unbound are universally quantified, unless
%	a)  they occur in the template
%	b)  they are bound by X^P, setof, or bagof
%   free_variables(Generator, Template, OldList, NewList)
%   finds this set, using OldList as an accumulator.

free_variables(Term, Bound, VarList, [Term|VarList]) :-
	var(Term),
	term_is_free_of(Bound, Term),
	list_is_free_of(VarList, Term),
	!.
free_variables(Term, Bound, VarList, VarList) :-
	var(Term),
	!.
free_variables(Term, Bound, OldList, NewList) :-
	explicit_binding(Term, Bound, NewTerm, NewBound),
	!,
	free_variables(NewTerm, NewBound, OldList, NewList).
free_variables(Term, Bound, OldList, NewList) :-
	functor(Term, _, N),
	free_variables(N, Term, Bound, OldList, NewList).

free_variables(0, Term, Bound, VarList, VarList) :- !.
free_variables(N, Term, Bound, OldList, NewList) :-
	arg(N, Term, Argument),
	free_variables(Argument, Bound, OldList, MidList),
	M is N-1, !,
	free_variables(M, Term, Bound, MidList, NewList).

%   explicit_binding checks for goals known to existentially quantify
%   one or more variables.  In particular \+ is quite common.

explicit_binding(\+ Goal,	       Bound, fail,	Bound      ) :- !.
explicit_binding(not(Goal),	       Bound, fail,	Bound	   ) :- !.
explicit_binding(Var^Goal,	       Bound, Goal,	Bound+Var) :- !.
explicit_binding(setof(Var,Goal,Set),  Bound, Goal-Set, Bound+Var) :- !.
explicit_binding(bagof(Var,Goal,Bag),  Bound, Goal-Bag, Bound+Var) :- !.
explicit_binding(set_of(Var,Goal,Set), Bound, Goal-Set, Bound+Var) :- !.
explicit_binding(bag_of(Var,Goal,Bag), Bound, Goal-Bag, Bound+Var) :- !.


term_is_free_of(Term, Var) :-
	var(Term), !,
	Term \== Var.
term_is_free_of(Term, Var) :-
	functor(Term, _, N),
	term_is_free_of(N, Term, Var).

term_is_free_of(0, Term, Var) :- !.
term_is_free_of(N, Term, Var) :-
	arg(N, Term, Argument),
	term_is_free_of(Argument, Var),
	M is N-1, !,
	term_is_free_of(M, Term, Var).


list_is_free_of([Head|Tail], Var) :-
	Head \== Var,
	!,
	list_is_free_of(Tail, Var).
list_is_free_of([], _).



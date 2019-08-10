:- module(wam_bootstrap_util, [lookup_atom/2, lookup_float/2, lookup_functor/3, lookup_dynamic_functor/3, emit_code/2, compile_message/1,
    ftable/2, fltable/2, atable/2, clause_table/5, fptable/2, ctable/2, itable/1, stable/1, dtable/1, '$module_export'/3,
    add_clause_to_predicate/3, add_clause_to_aux/4, add_clause_to_existing/2]).

:-dynamic(ftable/2). % functors
:-dynamic(fltable/2). % floats
:-dynamic(atable/2). % atoms
:-dynamic(clause_table/5). % clauses
:-dynamic(fptable/2). % foreign predicates
:-dynamic(ctable/2). % code
:-dynamic(itable/1). % initialization directive predicates
:-dynamic(stable/1). % system directive predicates
:-dynamic(dtable/1). % dynamic predicates
:-dynamic('$module_export'/3).

lookup_functor(Functor, Arity, N):-
        lookup_atom(Functor, F),
        ( ftable(F/Arity, N)->
            true
        ; otherwise->
            flag(ftable, N, N+1),
            assert(ftable(F/Arity, N))
        ).

lookup_dynamic_functor(Functor, Arity, N):-
        lookup_functor(Functor, Arity, N),
        ( dtable(N)->
            true
        ; otherwise->
            assert(dtable(N))
        ).

lookup_atom([], 0):- !.
lookup_atom(Atom, N):-
        ( atable(Atom, N)->
            true
        ; otherwise->
            flag(atable, N, N+1),
            assert(atable(Atom, N))
        ).

lookup_float(Float, N):-
        ( fltable(Float, N)->
            true
        ; otherwise->
            flag(fltable, N, N+1),
            assert(fltable(Float, N))
        ).

emit_code(N, Code):-
        assert(ctable(N, Code)).

/*
compile_message(A):-
    A = [H|T] -> write(H), compile_message(T)
    ;
    A = [] -> writeln('')
    ;
    writeln(A).
*/
compile_message(_).

add_clause_to_predicate(Name/Arity, Head, Body):-
        setof(N-Code, T^(ctable(T, Code), N is T /\ 0x7fffffff), SortedCodes),
        findall(Code, member(_-Code, SortedCodes), Codes),
        lookup_functor(Name, Arity, Predicate),
        ( retract(clause_table(Predicate, I, [254,0|PreviousCodes], HeadP, BodyP))->
            % If there is a NOP clause, then we have only one clause. Make it try_me_else, then add our new one as trust_me.
            II is I+1,
            assertz(clause_table(Predicate, I, [28, II|PreviousCodes], HeadP, BodyP)),
            assertz(clause_table(Predicate, II, [30, 0|Codes], Head, Body))
        ; retract(clause_table(Predicate, I, [30,0|PreviousCodes], HeadP, BodyP))->
            II is I+1,
            % If we have a trust_me, then make it retry_me_else (since there must have been another clause to try first, if we then have to trust_me)
            assertz(clause_table(Predicate, I, [29, II|PreviousCodes], HeadP, BodyP)),
            assertz(clause_table(Predicate, II, [30, 0|Codes], Head, Body))
        ; retract(clause_table(Predicate, 0, [], _HeadP, _BodyP))->
            % Predicate defined with no clauses, possibly due to dynamic directive defining Predicate.
            % add ours as a <NOP,0>
            assertz(clause_table(Predicate, 0, [254, 0|Codes], Head, Body))
        ; otherwise->
            % Otherwise Predicate not defined so there are no clauses yet. So just add ours as a <NOP,0>
            assertz(clause_table(Predicate, 0, [254, 0|Codes], Head, Body))
        ).

add_clause_to_aux(AuxLabel, N, L, LT):-
        ( nonvar(AuxLabel),
          AuxLabel = defined(A) ->
            NN is N xor 0x80000000,
            add_clause_to_existing(A, NN),
            L = LT
        ; otherwise->
            % Brand new aux! This gets <NOP,0> and sets L
            NN is N+1,
            assert(ctable(N, 254)),
            assert(ctable(NN, 0)),
            AuxLabel = defined(N),
            L = [label(AuxLabel, N)|LT]
        ).


add_clause_to_existing(A, N):-
        AA is A+1,
        NN is N+1,
        ( ctable(A, 254)->
            % Change <NOP,0> -> <try_me_else N>
            retract(ctable(A, _)),
            retract(ctable(AA, _)),
            assert(ctable(A, 28)),
            assert(ctable(AA, N)),
            % Add <trust_me, 0> at N
            assert(ctable(N, 30)),
            assert(ctable(NN, 0))
        ; ctable(A, 28)->
            % Follow path
            ctable(AA, Link),
            add_clause_to_existing(Link, N)
        ; ctable(A, 29)->
            % Follow link
            ctable(AA, Link),
            add_clause_to_existing(Link, N)
        ; ctable(A, 30)->
            % Change <trust_me, 0> -> <retry_me_else, N>
            retract(ctable(A, _)),
            retract(ctable(AA, _)),
            assert(ctable(A, 29)),
            assert(ctable(AA, N)),
            % Add <trust_me, 0> at N
            assert(ctable(N, 30)),
            assert(ctable(NN, 0))
        ).

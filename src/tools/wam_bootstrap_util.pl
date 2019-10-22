:- module(wam_bootstrap_util, [lookup_atom/2, lookup_float/2, lookup_functor/3, lookup_dynamic_functor/3, emit_code/2,
    compile_buffer_codes/1, compile_message/1,
    ftable/2, fltable/2, atable/2, clause_table/5, fptable/2, ctable/2, itable/1, stable/1, dtable/1,
    'pls$module_export'/3, 'pls$import'/2, 'pls$meta_predicate'/3, indexing_mode/1, set_indexing_mode/1,
    indexable_compiled_predicates/1, compiled_clauses/2, register_indexed_predicate/1, indexed_predicate/1,
    add_index_clause_to_predicate/1, edit_clauses_for_index_sequences/2,
    add_clause_to_predicate/3, add_clause_to_aux/4, add_clause_to_existing/2]).

:-dynamic(ftable/2). % functors
:-dynamic(fltable/2). % floats
:-dynamic(atable/2). % atoms
:-dynamic(clause_table/5). % clauses
:-dynamic(indexed_predicate/1). % predicate has been indexed (perhaps trivially - with no indexing changes).
:-dynamic(fptable/2). % foreign predicates
:-dynamic(ctable/2). % code
:-dynamic(itable/1). % initialization directive predicates
:-dynamic(stable/1). % system directive predicates
:-dynamic(dtable/1). % dynamic predicates
:-dynamic('pls$module_export'/3).
:-dynamic('pls$import'/2).
:-dynamic('pls$meta_predicate'/3).
:-dynamic('indexing_mode'/1).

lookup_functor(Functor, Arity, N):-
        integer(N)
          -> ftable(F/Arity, N),
             lookup_atom(Functor, F)
        ;
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
        ; var(N)->
            flag(atable, N, N+1),
            assert(atable(Atom, N))
        ).

lookup_float(Float, N):-
        ( fltable(Float, N)->
            true
        ; var(N)->
            flag(fltable, N, N+1),
            assert(fltable(Float, N))
        ).

emit_code(N, Code):-
        assert(ctable(N, Code)).

compile_buffer_codes(Codes) :-
    setof(N-Code, ctable(N, Code), Codes).

/*
compile_message(A):-
    A = [H|T] -> write(H), compile_message(T)
    ;
    A = [] -> writeln('')
    ;
    writeln(A).
*/
compile_message(_).

set_indexing_mode(New) :-
    mark_predicates_indexed,
    retractall('indexing_mode'(_)),
    assertz('indexing_mode'(New)).

mark_predicates_indexed :-
    indexable_compiled_predicates(Ps),
    forall(member(P, Ps), register_indexed_predicate(P)).

indexable_compiled_predicates(Ps) :-
    setof(P, A ^ B ^ C ^ D ^ Functor ^ Arity ^ (clause_table(P, A, B, C, D), \+dtable(P), \+ indexed_predicate(P), ftable(Functor/Arity, P), Arity > 0), Ps)
      -> true
    ;
    Ps = [].

register_indexed_predicate(P) :-
    retractall(indexed_predicate(P)),
    assertz(indexed_predicate(P)).

compiled_clauses(PredicateID, Clauses) :-
    setof(ClauseOffset-Head/ClauseCodes,
        Body^clause_table(PredicateID, ClauseOffset, ClauseCodes, Head, Body),
        Clauses).

add_index_clause_to_predicate(Predicate) :-
        setof(N-Code, T^(ctable(T, Code), N is T /\ 0x7fffffff), SortedCodes),
        findall(Code, member(_-Code, SortedCodes), Codes),
        setof(ID, OtherCodes^Head^Body^clause_table(Predicate, ID, OtherCodes, Head, Body), IDs),
        append(_, [MaxID], IDs),
        IndexID is MaxID + 1,
        assertz(clause_table(Predicate, IndexID, Codes, index, index)).

edit_clauses_for_index_sequences([], _).
edit_clauses_for_index_sequences([H|T], Predicate) :-
        edit_clauses_for_index_sequence(H, Predicate),
        edit_clauses_for_index_sequences(T, Predicate).

edit_clauses_for_index_sequence([Only], Predicate) :-
        modify_clause_control_instruction(Only, [254,0], Predicate). % nop2
edit_clauses_for_index_sequence([First, Second|Rest], Predicate) :-
        modify_clause_control_instruction(First, [28,Second], Predicate), % try_me_else
        edit_clauses_for_index_sequence1([Second|Rest], Predicate).

edit_clauses_for_index_sequence1([Last], Predicate) :-
        modify_clause_control_instruction(Last, [30,0], Predicate). % trust_me
edit_clauses_for_index_sequence1([First, Second|Rest], Predicate) :-
        modify_clause_control_instruction(First, [29,Second], Predicate), % retry_me_else
        edit_clauses_for_index_sequence1([Second|Rest], Predicate).

modify_clause_control_instruction(Index, Control, Predicate) :-
        retract(clause_table(Predicate, Index, [_,_|BodyCodes], Head, Body)),
        append(Control, BodyCodes, FullCodes),
        assertz(clause_table(Predicate, Index, FullCodes, Head, Body)).

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

/*
create large memory_file of Prolog facts and copy to local storage.
copy local storage to new memory_file and compile.

test facts: local_storage_test:test(1,1)...lstest:test(N, N).
*/

:- module(test_local_storage, [test/0, test/1]).

:- use_module(library(wam_compiler)).

:- meta_predicate(write_goal((:),?)).

:- dynamic(data/2).
:- dynamic(local_storage_key/2).
:- dynamic(display_spans_mode/1).

test :-
    test(1, 1).

test(K, N) :-
    asserta(display_spans_mode(all)),
    dom_window(W),
    dom_object_property(_, W, localStorage, S),
    %forall(retract(local_storage_key(test_local_storage, OldKey)), dom_object_method(S, removeItem(OldKey))),
    retractall(data(_,_)),
    save_to_local_storage(K, N, test_local_storage),
    load_from_local_storage(test_local_storage),
    findall(X-ID, data(X,ID), IDs),
    length(IDs, L),
    writeln(test(K, N,L)).

save_to_local_storage(0, _NumberOfFacts, _KeyPrefix) :- !.
save_to_local_storage(NumberOfKeys, NumberOfFacts, KeyPrefix) :-
    NumberOfKeys > 0,
    format(atom(Key), '~w-~w', [KeyPrefix, NumberOfKeys]),
    save_to_local_storage1(NumberOfKeys, NumberOfFacts, Key),
    assertz(local_storage_key(KeyPrefix, Key)),
    Next is NumberOfKeys - 1,
    save_to_local_storage(Next, NumberOfFacts, KeyPrefix).


save_to_local_storage1(NumberOfKeys, NumberOfFacts, Key) :-
    new_memory_file(DataMemFile),
    open_memory_file(DataMemFile, write, Stream),
    write_test_facts(Stream, NumberOfFacts, NumberOfKeys),
    close(Stream),
    copy_memory_file_to_local_storage(DataMemFile, Key),
    free_memory_file(DataMemFile).


write_test_facts(_, 0, _) :- !.
write_test_facts(Stream, N, K) :-
    N > 0,
    write_goal(data(K, N), Stream),
    NN is N - 1,
    write_test_facts(Stream, NN, K).

write_goal(H, Stream) :-
    writeq(Stream, H),
    write(Stream, '.\n').

load_from_local_storage(Key) :-
    retractall(data(_, _)),
    findall(K, local_storage_key(Key, K), Ks),
    load_from_local_storage1(Ks).

load_from_local_storage1([]).
load_from_local_storage1([K|Ks]) :-
    copy_local_storage_to_memory_file(K, DataMemFile),
    compile_and_free_memory_file(DataMemFile),
    load_from_local_storage1(Ks).

display_spans(Markers, Functor) :-
    (display_spans_mode(all);display_spans_mode(Functor))
      ->  spans(Markers, Spans),
          Structure =.. [Functor|Spans],
          alert(Structure)
    ;
    true.

spans([H1, H2|T], Spans) :-
    spans([H2|T], H1, Spans).

spans([], _, []).
spans([H|T], R, [Span|TS]) :-
    Span is H - R,
    spans(T, H, TS).

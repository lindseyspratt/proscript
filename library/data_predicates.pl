/*
This file predicates support storing data by asserting predicates.
The naming of the data predicates is handled by data_predicate_dynamics(Definitions)
where Definitions is a list of data predicate definitions.
Each definition is a pair of terms Type-Attributes where Type is the 'type' of the data
and Attributes is a list of attributes of instances of this type.
This Type and Attributes are defined using a data_predicates(ShadowType, Type, Attributes).
The ShadowType is generally a shorthand for the Type and is used when specifying instances
to be asserted.

Address data could be defined as
    data_predicates(addr, address, [name, street_number, street_name, city, state, zip]).
data_predicate_dynamics([address-[name, street_number, street_name, city, state, zip]]) creates several dynamic predicates:
address_name/2, address_street_number/2, address_street_name/2, etc.
Also it creates arity 1 versions of these predicates:
address_name/1, address_street_number/1, address_street_name/1, etc.
The arity 2 predicates associate attributes with an 'instance' of the data type.
For the address example we can assert an address using assert_datas([addr(nguyen, 15, main_st, centerville, idaho, 83631)]).
This associates ID 1 with each of these attributes: address_name(1, nguyen), address_street_number(1, 15), etc.
The default_asserted_id(Type, ID) is updated to the last data asserted which is 1 in this example.
*/
:- module(data_predicates,
    [data_predicates/3, data_predicate_dynamics/1,
    assert_datas/1, assert_datas/2, assert_id_datas/1, assert_data/2,
    labelled_values/3, retract_all_data/1, retract_data/2,
    save_data_stream/2, save_data_stream/3,
    save_data/2, save_data/3, load_data/2, remove_data/2]).

:- use_module(library(wam_compiler)).

:- meta_predicate((
    data_predicate_dynamics((:)),
    assert_data((:),?),
    assert_id_datas((:)),
    assert_datas((:), ?),
    assert_datas((:)),
    data_predicates(?, (:), ?),
    labelled_values((:), ?, ?),
    retract_data((:),?),
    retract_all_data((:)),
    retract_all_data((:), ?),
    save_data((:),?),
    save_data((:),?,?),
    save_data_stream((:),?),
    save_data_stream((:),?,?),
    gather_data1(?,(:),?),
    load_data((:), ?),
    remove_data((:), ?))).

:- dynamic data_predicates/3.

% ShadowData is in-memory representation of data
% where ShadowData is a structure of many arguments and
% data is stored as a collection of binary relations where
% each fact shares the same identifier as the first argument.

assert_datas(Datas) :-
    assert_datas(Datas, 1).

assert_datas(_M1:M2:Term, ID) :-
    !,
    assert_datas(M2:Term, ID).
assert_datas(Term, ID) :-
    assert_datas1(Term, ID).

assert_datas1(_M:[], _).
assert_datas1(M:[H|T], J) :-
    assert_data1(M:H, J),
    K is J + 1,
    assert_datas1(M:T, K).

assert_id_datas(_M1:M2:Term) :-
    !,
    assert_id_datas(M2:Term).
assert_id_datas(Term) :-
    assert_id_datas1(Term).

assert_id_datas1(_M:[]).
% This predicate body usew assert_data1/2 instead of assert_data/2.
% This is valid because of explicit Module info (in M:H).
% This is desireable because it is more efficient by avoiding the extra layer of predicate invocation.
assert_id_datas1(M:[H-ID|T]) :-
    assert_data1(M:H, ID),
    assert_id_datas1(M:T).

assert_data(_M1:M2:ShadowData, ID) :-
    !,
    assert_data(M2:ShadowData, ID).
assert_data(ShadowData, ID) :-
    assert_data1(ShadowData, ID).

assert_data1(M:ShadowData, ID) :-
    ShadowData =.. [F|Args],
    data_predicates(F, M:Prefix, Suffixes),
    (length(Suffixes, ArgCount),
     length(Args, ArgCount)
       -> assert_shadow_arguments(Args, M:Prefix, Suffixes, ID),
          default_asserted_id(M:Prefix, ID)
    ;
     length(Suffixes, ArgCount1),
     length(Args, ArgCount2),
     throw(shadow_argument_error('Count different from count of shadow argument', M, F, Prefix, ArgCount1, ArgCount2))
    ).

assert_shadow_arguments([], _, [], _).
assert_shadow_arguments([H|T], MPrefix, [HP|TP], ID) :-
    assert_shadow_argument(H, MPrefix, HP, ID),
    assert_shadow_arguments(T, MPrefix, TP, ID).

assert_shadow_argument(Arg, M:Prefix, Suffix, ID) :-
    construct_data_predicate(Prefix, Suffix, Predicate),
    GoalR =.. [Predicate, ID, _],
    Goal =.. [Predicate, ID, Arg],
    retractall(M:GoalR),
    assertz(M:Goal).

default_asserted_id(M:Prefix, ID) :-
    atom_concat(Prefix, '_default_id', Predicate),
    GoalR =.. [Predicate, _],
    Goal =.. [Predicate, ID],
    retractall(M:GoalR),
    assertz(M:Goal).

retract_default_id(M:Prefix) :-
    atom_concat(Prefix, '_default_id', Predicate),
    GoalR =.. [Predicate, _],
    retractall(M:GoalR).

retract_default_id(M:Prefix, ID) :-
    atom_concat(Prefix, '_default_id', Predicate),
    Goal =.. [Predicate, ID],
    retract(M:Goal).

write_default_id(M:Prefix, Stream) :-
    atom_concat(Prefix, '_default_id', Predicate),
    Goal =.. [Predicate, _],
    call(M:Goal)
      -> write_goal(M:Goal, Stream)
    ;
    true.

data_predicate_dynamics(M : DPs) :-
    assert_dps(M : DPs),
    findall(Prefix-Suffixes, data_predicates(_, M:Prefix, Suffixes), All),
    data_predicate_dynamics1a(M:All).

assert_dps(_M : []).
assert_dps(M : [H|T]) :-
    assert_dp(M : H),
    assert_dps(M : T).

assert_dp(M : data_predicates(F, P, S)) :-
    assertz(data_predicates(F, M:P, S)).

data_predicate_dynamics1a(_M:[]).
data_predicate_dynamics1a(M:[Prefix-Suffixes|T]) :-
    data_predicate_dynamics(Suffixes, M:Prefix),
    atom_concat(Prefix, '_default_id', DefaultPredicate),
    (dynamic(M:(DefaultPredicate / 1))),
    data_predicate_dynamics1a(M:T).

data_predicate_dynamics([], _).
data_predicate_dynamics([H|T], MPrefix) :-
    data_predicate_dynamic(H, MPrefix),
    data_predicate_dynamics(T, MPrefix).

data_predicate_dynamic(Suffix, M:Prefix) :-
    construct_data_predicate(Prefix, Suffix, Predicate),
    (dynamic(M:(Predicate / 2))), % BUG: 'dynamic(Predicate/2), foo' is being parsed as dynamic (Predicate/2, foo).
    data_predicate_default_dynamic(M:Prefix, Predicate).

data_predicate_default_dynamic(M:Prefix, Predicate) :-
    (dynamic(M:(Predicate /1))), % BUG: 'dynamic(Predicate/1), foo' is being parsed as dynamic (Predicate/1, foo).
    Head =.. [Predicate, Value],

    retractall(M:Head :- _),
    atom_concat(Prefix, '_default_id', DefaultPredicate),
    DefaultGoal =.. [DefaultPredicate, DefaultID],
    BinaryGoal =.. [Predicate, DefaultID, Value],
    asserta((M:Head :- M:DefaultGoal, M:BinaryGoal)).

construct_data_predicate(Prefix, Suffix, Predicate) :-
    atom_concat(Prefix, '_', PrefixExtended),
    atom_concat(PrefixExtended, Suffix, Predicate).

labelled_values(M:Prefix, ID, Values) :-
    data_predicates(_, M:Prefix, Suffixes),
    data_values(Suffixes, M:Prefix, ID, Values).

data_values([], _, _, []).
data_values([H|T], M:Prefix, ID, [H-HV|TV]) :-
    data_value(H, M:Prefix, ID, HV),
    data_values(T, M:Prefix, ID, TV).

data_value(Suffix, M:Prefix, ID, Value) :-
    construct_data_predicate(Prefix, Suffix, Predicate),
    Goal =.. [Predicate, ID, Value],
    call(M:Goal).

retract_all_data(_M1:M2:Prefix) :-
    !,
    retract_all_data(M2:Prefix).
retract_all_data(DataSpec) :-
    retract_all_data1(DataSpec).

retract_all_data1(M:Prefix) :-
    retract_default_id(M:Prefix),
    ids_in_use(M:Prefix, SortedIDs),
    retract_all_data1(SortedIDs, M:Prefix).

retract_all_data(_M1:M2:Prefix, IDs) :-
    !,
    retract_all_data(M2:Prefix, IDs).
retract_all_data(DataSpec, IDs) :-
    retract_all_data1(IDs, DataSpec).

retract_all_data1([],_).
retract_all_data1([H|T], M:Prefix) :-
    retract_data1(M:Prefix, H),
    retract_all_data1(T, M:Prefix).

retract_data(_M1:M2:Prefix, ID) :-
    !,
    retract_data(M2:Prefix, ID).
retract_data(DataSpec, ID) :-
    retract_data1(DataSpec, ID).

retract_data1(M:Prefix, ID) :-
    (retract_default_id(M:Prefix, ID) % retract foo_default_id if set to ID, otherwise leave it.
      -> true
    ;
     true
    ),
    data_predicates(_, M:Prefix, Suffixes),
    retract_data(Suffixes, M:Prefix, ID).

retract_data([], _, _).
retract_data([H|T], M:Prefix, ID) :-
    construct_data_predicate(Prefix, H, Predicate),
    Goal =.. [Predicate, ID, _],
    retractall(M:Goal),
    retract_data(T, M:Prefix, ID).

% store facts for data matching M:Prefix in the browser's localStorage
% where the facts are stored as a single string/blob and the
% localStorage key is a concatenation of Key and M:Prefix.

save_data(_M1:M2:Prefix, Target) :-
    !,
    save_data(M2:Prefix, Target).
save_data(DataSpec, Target) :-
    save_data1(DataSpec, Target).

save_data1(M:Prefix, local_storage(Key)) :-
    ids_in_use(M:Prefix, IDs),
    save_data1(M:Prefix, IDs, local_storage(Key)).

save_data(_M1:M2:Prefix, IDs, Target) :-
    !,
    save_data(M2:Prefix, IDs, Target).
save_data(DataSpec, IDs, Target) :-
    save_data1(DataSpec, IDs, Target).

save_data1(M:Prefix, IDs, local_storage(Key)) :-
    new_memory_file(DataMemFile),
    open_memory_file(DataMemFile, write, Stream),
    % write foo_default_id(ID) ...
    write_default_id(M:Prefix, Stream),
    gather_data(Stream, M:Prefix, IDs),
    close(Stream),
    format(atom(K), '~w/~w', [Key, M:Prefix]),
    copy_memory_file_to_local_storage(DataMemFile, K),
    free_memory_file(DataMemFile).

save_data_stream(_M1:M2:Prefix, Stream) :-
    !,
    save_data_stream(M2:Prefix, Stream).
save_data_stream(DataSpec, Stream) :-
    save_data_stream1(DataSpec, Stream).

save_data_stream(_M1:M2:Prefix, IDs, Stream) :-
    !,
    save_data_stream(M2:Prefix, IDs, Stream).
save_data_stream(DataSpec, IDs, Stream) :-
    save_data_stream1(DataSpec, IDs, Stream).

save_data_stream1(M:Prefix, Stream) :-
    ids_in_use(M:Prefix, IDs),
    save_data_stream1(M:Prefix, IDs, Stream).

save_data_stream1(M:Prefix, IDs, Stream) :-
    % write foo_default_id(ID) ...
    write_default_id(M:Prefix, Stream),
    gather_data(Stream, M:Prefix, IDs).

gather_data(Stream, M:Prefix, IDs) :-
    data_predicates(_, M:Prefix, Suffixes),
    gather_data(Suffixes, Stream, M:Prefix, IDs).

gather_data([], _, _, _).
gather_data([H|T], Stream, M:Prefix, IDs) :-
    construct_data_predicate(Prefix, H, Predicate),
    write(Stream, '\n% '), write(Stream, M:Predicate), write(Stream, '\n\n'),
    gather_data1(IDs, M:Predicate, Stream),
    gather_data(T, Stream, M:Prefix, IDs).

gather_data1([], _Predicate, _Stream).
gather_data1([H|T], M:Predicate, Stream) :-
    (setof(M:Goal, X^(Goal =.. [Predicate, H, X], call(M:Goal)), Goals)
      -> write_goals(Goals, Stream)
    ;
     true
    ),
    gather_data1(T, M:Predicate, Stream).

write_goals([], _Stream).
write_goals([H|T], Stream) :-
    write_goal(H, Stream),
    write_goals(T, Stream).

% writeq/2 is used to ensure that the written
% goal is readable by the Prolog parser.
% This does not use write_canonical/2 because
% that predicate explodes lists (e.g. [a,b] becomes
% '.'(a,'.'(b,[]))) which could cause the saved
% goals to require a great deal more storage.
% Since write_canonical/2 is not used then whatever
% operator definitions are in place when these goals are
% written can affect the layout of these goals.
% Care must be taken that the appropriate operator
% definitions are in place when loading these goals.

write_goal(H, Stream) :-
    writeq(Stream, H),
    write(Stream, '.\n').

load_data(_M1:M2:Prefix, Source) :-
    !,
    load_data(M2:Prefix, Source).
load_data(DataSpec, Source) :-
    load_data1(DataSpec, Source).

load_data1(M:Prefix, local_storage(Key)) :-
    writeln(load_data1(M:Prefix, local_storage(Key))),
    writeln(load_data1(retractall)),
    retract_all_data1(M:Prefix),
    format(atom(K), '~w/~w', [Key, M:Prefix]),
    writeln(load_data1(copy)),
    copy_local_storage_to_memory_file(K, DataMemFile),
    writeln(load_data1(compile)),
    compile_and_free_memory_file(DataMemFile),
    writeln(load_data1(completed)).

remove_data(_M1:M2:Prefix, Store) :-
    !,
    remove_data(M2:Prefix, Store).
remove_data(DataSpec, Store) :-
    remove_data1(DataSpec, Store).

remove_data1(M:Prefix, local_storage(Key)) :-
    format(atom(K), '~w/~w', [Key, M:Prefix]),
    dom_window(W),
    dom_object_property(_, W, localStorage, S),
    dom_object_method(S, removeItem(K)).

ids_in_use(M:Prefix, SortedIDs) :-
    data_predicates(_, M:Prefix, Suffixes),
    !,
    ids_in_use(Suffixes, M:Prefix, IDs),
    sort(IDs, SortedIDs). % remove duplicates.

ids_in_use([], _, []).
ids_in_use([H|T], M:Prefix, IDs) :-
    construct_data_predicate(Prefix, H, Predicate),
    (ids_in_use1(M:Predicate, LocalIDs)
      -> append(LocalIDs, NextIDs, IDs)
    ;
     NextIDs = IDs
    ),
    ids_in_use(T, M:Prefix, NextIDs).

ids_in_use1(M:Predicate, IDs) :-
    setof(ID, X^Goal^(Goal =.. [Predicate, ID, X], call(M:Goal)), IDs).

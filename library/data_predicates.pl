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
The default_asserted_id(Type, ID, Opt) is updated to the last data asserted which is 1 in this example.
*/
:- module(data_predicates,
    [data_predicates/4, data_predicate_dynamics/1,
    assert_datas/1, assert_datas/2, assert_id_datas/1, assert_data/2,
    labelled_values/3, retract_all_data/1, retract_data/2,
    save_data_stream/2, save_data_stream/3,
    save_data/2, save_data/3, load_data/2, remove_data/2]).

:- use_module(library(wam_compiler)).
:- use_module(undo).

:- meta_predicate((
    data_predicate_dynamics((:)),
    assert_data((:),?),
    assert_id_datas((:)),
    assert_datas((:), ?),
    assert_datas((:)),
    data_predicates(?, (:), ?, ?),
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

:- dynamic data_predicates/4.

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
    data_predicates(F, M:Prefix, Opt, Suffixes),
    (length(Suffixes, ArgCount),
     length(Args, ArgCount)
       -> assert_shadow_arguments(Args, M:Prefix, Suffixes, ID, Opt),
          default_asserted_id(M:Prefix, ID, Opt)
    ;
     length(Suffixes, ArgCount1),
     length(Args, ArgCount2),
     throw(shadow_argument_error('Count different from count of shadow argument', M, F, Prefix, ArgCount1, ArgCount2))
    ).

assert_shadow_arguments([], _, [], _, _).
assert_shadow_arguments([H|T], MPrefix, [HP|TP], ID, Opt) :-
    assert_shadow_argument(H, MPrefix, HP, ID, Opt),
    assert_shadow_arguments(T, MPrefix, TP, ID, Opt).

assert_shadow_argument(Arg, M:Prefix, Suffix, ID, Opt) :-
    construct_data_predicate(Prefix, Suffix, Predicate),
    storage_mode(Opt, Mode),
    set_abstract(Mode, M:Predicate, ID, Arg).

storage_mode(Opt, Mode) :-
    member(undoable, Opt)
      -> (member(ephemeral, Opt)
           -> throw(incompatible_data_predicates_options(ephemeral_and_undoable, Opt))
         ;
         Mode = undoable
         )
    ;
    member(ephemeral, Opt)
      -> (member(undoable, Opt)
           -> throw(incompatible_data_predicates_options(ephemeral_and_undoable, Opt))
         ;
         Mode = ephemeral
         )
    ;
    Mode = ephemeral.

set_abstract(ephemeral, M:Predicate, ID) :-
    GoalR =.. [Predicate, _],
    Goal =.. [Predicate, ID],
    retractall(M:GoalR),
    assertz(M:Goal).
set_abstract(undoable, M:Predicate, ID) :-
    Goal =.. [Predicate, _],
    GoalNew =.. [Predicate, ID],
    undoable_update(M:Goal, M:GoalNew).

set_abstract(ephemeral, M:Predicate, ID, Arg) :-
    GoalR =.. [Predicate, ID, _],
    Goal =.. [Predicate, ID, Arg],
    retractall(M:GoalR),
    assertz(M:Goal).
set_abstract(undoable, M:Predicate, ID, Arg) :-
    GoalNew =.. [Predicate, ID, Arg],
    Goal =.. [Predicate, ID, _],
    undoable_update(M:Goal, M:GoalNew).

retract_abstract(ephemeral, M:Predicate, ID) :-
    GoalR =.. [Predicate, ID],
    retract(M:GoalR).
retract_abstract(undoable, M:Predicate, ID) :-
    GoalR =.. [Predicate, ID],
    undoable_retract(M:GoalR).

retract_abstract(ephemeral, M:Predicate, ID, Arg) :-
    GoalR =.. [Predicate, ID, Arg],
    retract(M:GoalR).
retract_abstract(undoable, M:Predicate, ID, Arg) :-
    GoalR =.. [Predicate, ID, Arg],
    undoable_retract(M:GoalR).

default_asserted_id(M:Prefix, ID, Opt) :-
    atom_concat(Prefix, '_default_id', Predicate),
    storage_mode(Opt, Mode),
    set_abstract(Mode, M:Predicate, ID).

retract_default_id(M:Prefix, ID, Opt) :-
    atom_concat(Prefix, '_default_id', Predicate),
    storage_mode(Opt, Mode),
    retract_abstract(Mode, M:Predicate, ID).

write_default_id(M:Prefix, Stream) :-
    atom_concat(Prefix, '_default_id', Predicate),
    Goal =.. [Predicate, _],
    call(M:Goal)
      -> write_goal(M:Goal, Stream)
    ;
    true.

data_predicate_dynamics(M : DPs) :-
    assert_dps(M : DPs),
    findall(Prefix-Suffixes-Opt, data_predicates(_, M:Prefix, Opt, Suffixes), All),
    data_predicate_dynamics1a(M:All).

assert_dps(_M : []) :- !.
assert_dps(M : [H|T]) :-
    assert_dp(M : H),
    assert_dps(M : T).

assert_dp(M : data_predicates(F, P, S)) :-
    !,
    assert_dp(M : data_predicates(F, P, [], S)).

assert_dp(M : data_predicates(F, P, Opt, S)) :-
    data_predicates(_, M:P, _, _)
      -> true % writeln(already_asserted_long(M:P))
    ;
    assertz(data_predicates(F, M:P, Opt, S)).

data_predicate_dynamics1a(_M:[]).
data_predicate_dynamics1a(M:[Prefix-Suffixes-Opt|T]) :-
    data_predicate_dynamics(Suffixes, M:Prefix, Opt),
    atom_concat(Prefix, '_default_id', DefaultPredicate),
    (dynamic(M:(DefaultPredicate / 1))),
    data_predicate_dynamics1a(M:T).

data_predicate_dynamics([], _, _).
data_predicate_dynamics([H|T], MPrefix, Opt) :-
    data_predicate_dynamic(H, MPrefix, Opt),
    data_predicate_dynamics(T, MPrefix, Opt).

data_predicate_dynamic(Suffix, M:Prefix, Opt) :-
    construct_data_predicate(Prefix, Suffix, Predicate),
    (dynamic(M:(Predicate / 2))), % BUG: 'dynamic(Predicate/2), foo' is being parsed as dynamic (Predicate/2, foo).
    data_predicate_default_dynamic(M:Prefix, Predicate),
    storage_mode(Opt, Mode),
    assert_action_predicates(M:Predicate, Mode).

data_predicate_default_dynamic(M:Prefix, Predicate) :-
    (dynamic(M:(Predicate /1))), % BUG: 'dynamic(Predicate/1), foo' is being parsed as dynamic (Predicate/1, foo).
    assert_unary_predicate(M:Prefix, Predicate).

assert_unary_predicate(ModulePrefix, Predicate) :-
    build_unary_predicate_clause(ModulePrefix, Predicate, Clause),
    retract_assert_clause(Clause).

build_unary_predicate_clause(M:Prefix, Predicate, Clause) :-
    create_clause(
        goal(M, [Predicate], [Arg]),
        (goal(M, [Prefix, default_id], [ID]),
         goal(M, [Predicate], [ID,Arg]),
         !
        ),
        Clause).

assert_action_predicates(ModulePredicate, Mode) :-
    assert_action_predicate(get, ModulePredicate, Mode),
    assert_action_predicate(set, ModulePredicate, Mode),
    assert_action_predicate(update, ModulePredicate, Mode),
    assert_action_predicate(clear, ModulePredicate, Mode),
    assert_dummy_reference(ModulePredicate).

assert_action_predicate(Action, ModulePredicate, Mode) :-
    build_predicate_clause(Action, Mode, ModulePredicate, Clauses),
    retract_assert_clauses(Clauses).

retract_assert_clauses([FirstClause|OtherClauses]) :-
    retract_assert_clause(FirstClause),
    assert_clauses(OtherClauses).

retract_assert_clause(Module:Head :- Body) :-
    functor(Head, Functor, Arity),
    (current_predicate(Module:Functor/Arity)
      ->  functor(TemplateHead, Functor, Arity),
          retractall(Module:TemplateHead :- _)
    ;
    true
    ),
    asserta(Module:Head :- Body).

assert_clauses([]).
assert_clauses([H|T]) :-
    asserta(H),
    assert_clauses(T).

assert_dummy_reference(M:Predicate) :-
    create_goals(goal(M, [dummy_reference, Predicate], []), DummyReference),
    create_clause(
        DummyReference,
        (throw(invalid_clause(DummyReference)),
         DummyReference,
         goal(M, [get, Predicate], [_, _]),
         goal(M, [set, Predicate], [_, _]),
         goal(M, [clear, Predicate], [_])
        ),
        Clause),
    retract_assert_clause(Clause).

/*
dummy_reference :-
    throw(dummy_reference),
    M:set_Prefix_Suffix(_, _).

M:set_Prefix_Suffix(ID, Arg) :-
    update_Prefix_Suffix(ID, _, Arg).

% ephemeral
M:set_Prefix_Suffix(ID, Arg) :-
    retractall(M:Prefix_Suffix(ID, _)),
    asserta(M:Prefix_Suffix(ID, Arg)).

% undoable
M:update_Prefix_Suffix(ID, Old, New) :-
    M:Prefix_Suffix(ID, Old)
      -> undoable_update(M:Prefix_Suffix(ID, Old), M:Prefix_Suffix(ID, New))
    ;
    undoable_assert(M:Prefix_Suffix(ID, New)).

% ephemeral
M:update_Prefix_Suffix(ID, Old, New) :-
    retract(M:Prefix_Suffix(ID, Old)),
    asserta(M:Prefix_Suffix(ID, New)).

% undoable
M:clear_Prefix_Suffix(ID) :-
    undoable_retract(M:Prefix_Suffix(ID, _)).

% ephemeral
M:clear_Prefix_Suffix(ID) :-
    retractall(M:Prefix_Suffix(ID, _)).

*/

build_predicate_clause(get, _, M:Predicate, [Clause1, Clause2]) :-
    create_clause(
        goal(M, [get, Predicate], [ID, Arg]),
        (ground(ID),
        goal(M, [Predicate], [ID, Arg]),
        !),
        Clause1),
    create_clause(
        goal(M, [get, Predicate], [ID, Arg]),
        (var(ID),
        goal(M, [Predicate], [ID, Arg])
        ),
        Clause2).

build_predicate_clause(set, _, M:Predicate, [Clause]) :-
    create_clause(
        goal(M, [set, Predicate], [ID, Arg]),
        goal(M, [update, Predicate], [ID, _, Arg]),
        Clause).

build_predicate_clause(update, undoable, M:Predicate, [Clause]) :-
    create_goals(goal(M, [Predicate], [ID, Old]), RefOld),
    create_goals(goal(M, [Predicate], [ID, New]), RefNew),
    create_clause(
        goal(M, [update, Predicate], [ID, Old, New]),
        (RefOld -> undo:undoable_update(RefOld, RefNew) ; Old = '$none', undo:undoable_assert(RefNew)),
        Clause).

build_predicate_clause(update, ephemeral, M:Predicate, [Clause]) :-
    create_goals(goal(M, [Predicate], [ID, Old]), RefOld),
    create_goals(goal(M, [Predicate], [ID, New]), RefNew),
    create_clause(
        goal(M, [update, Predicate], [ID, Old, New]),
        ((retract(RefOld)->true;Old='$none'), asserta(RefNew)),
        Clause).

build_predicate_clause(clear, undoable, M:Predicate, [Clause]) :-
    create_goals(goal(M, [Predicate], [ID, _]), RefNew),
    create_clause(
        goal(M, [clear, Predicate], [ID]),
        undo:undoable_retract(RefNew),
        Clause).

build_predicate_clause(clear, ephemeral, M:Predicate, [Clause]) :-
    create_goals(goal(M, [Predicate], [ID, _]), RefNew),
    create_clause(
        goal(M, [clear, Predicate], [ID]),
        undo:undoable_retract(RefNew),
        Clause).

create_clause(Head, Body, (CreatedHead :- CreatedBody)) :-
    create_goals(Head, CreatedHead),
    create_goals(Body, CreatedBody).

create_goal(goal(Module, Parts, Args), Module:CreatedGoal) :-
    concat_parts(Parts, '_', Predicate),
    CreatedGoal =.. [Predicate|Args ].

create_goals((A,B), (CreatedA, CreatedB)) :-
    !,
    create_goals(A, CreatedA),
    create_goals(B, CreatedB).
create_goals((A -> B ; C), (CreatedA -> CreatedB ; CreatedC)) :-
    !,
    create_goals(A, CreatedA),
    create_goals(B, CreatedB),
    create_goals(C, CreatedC).
create_goals((A ; C), (CreatedA ; CreatedC)) :-
    !,
    create_goals(A, CreatedA),
    create_goals(C, CreatedC).
create_goals(goal(Module, Parts, Args), CreatedGoal) :-
    !,
    create_goal(goal(Module, Parts, Args), CreatedGoal).
create_goals(Goal, Goal).

concat_parts(Parts, Separator, Concatenation) :-
    concat_parts(Parts, Separator, '', Concatenation).

concat_parts([], _, Out, Out).
concat_parts([H|T], Separator, In, Out) :-
    (In = ''
      -> Next = H
    ;
    Separator = ''
      -> atomic_concat(In, H, Next)
    ;
    atomic_concat(In, Separator, ExtendedIn),
    atom_concat(ExtendedIn, H, Next)
    ),
    concat_parts(T, Separator, Next, Out).

atomic_concat(A, B, C) :-
    (number(A)
      -> number_codes(A, ACodes),
         atom_codes(AAtom, ACodes)
    ;
    AAtom = A
    ),
    (number(B)
      -> number_codes(B, BCodes),
         atom_codes(BAtom, BCodes)
    ;
    BAtom = B
    ),
    atom_concat(AAtom, BAtom, C).

construct_data_predicate(Prefix, Suffix, Predicate) :-
    atom_concat(Prefix, '_', PrefixExtended),
    atom_concat(PrefixExtended, Suffix, Predicate).

labelled_values(M:Prefix, ID, Values) :-
    data_predicates(_, M:Prefix, _, Suffixes),
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
    ids_in_use(M:Prefix, SortedIDs),
    retract_all_data1(SortedIDs, M:Prefix).

retract_all_data(_M1:M2:Prefix, IDs) :-
    !,
    retract_all_data(M2:Prefix, IDs).
retract_all_data(DataSpec, IDs) :-
    retract_all_data1(IDs, DataSpec).

retract_all_data1([], _).
retract_all_data1([H|T], M:Prefix) :-
    retract_data1(M:Prefix, H),
    retract_all_data1(T, M:Prefix).

retract_data(_M1:M2:Prefix, ID) :-
    !,
    retract_data(M2:Prefix, ID).
retract_data(DataSpec, ID) :-
    retract_data1(DataSpec, ID).

retract_data1(M:Prefix, ID) :-
    data_predicates(_, M:Prefix, Opt, Suffixes),
    (retract_default_id(M:Prefix, ID, Opt) % retract foo_default_id if set to ID, otherwise leave it.
      -> true
    ;
     true
    ),
    retract_data(Suffixes, M:Prefix, ID, Opt).

retract_data([], _, _, _).
retract_data([H|T], M:Prefix, ID, Opt) :-
    construct_data_predicate(Prefix, H, Predicate),
    storage_mode(Opt, Mode),
    retract_abstract(Mode, M:Predicate, ID, _),
    retract_data(T, M:Prefix, ID, Opt).

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
    data_predicates(_, M:Prefix, _, Suffixes),
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
    data_predicates(_, M:Prefix, _, Suffixes),
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

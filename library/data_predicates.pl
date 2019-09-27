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
    labelled_values/3]).

:- meta_predicate((
    data_predicate_dynamics((:)),
    assert_data((:),?),
    assert_id_datas((:)),
    assert_datas((:), ?),
    assert_datas((:)),
    data_predicates(?, (:), ?),
    labelled_values((:), ?, ?))).

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
     throw('tile shadow argument count different from count of shadow argument tile_predicates')
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

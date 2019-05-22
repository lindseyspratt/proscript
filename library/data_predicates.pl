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

:- dynamic data_predicates/3.

% ShadowData is in-memory representation of data
% where ShadowData is a structure of many arguments and
% data is stored as a collection of binary relations where
% each fact shares the same identifier as the first argument.

assert_datas(Datas) :-
    assert_datas(Datas, 1).

assert_datas([], _).
assert_datas([H|T], J) :-
    assert_data(H, J),
    K is J + 1,
    assert_datas(T, K).

assert_id_datas([]).
assert_id_datas([H-ID|T]) :-
    assert_data(H, ID),
    assert_id_datas(T).

assert_data(ShadowData, ID) :-
    ShadowData =.. [F|Args],
    data_predicates(F, Prefix, Suffixes),
    (length(Suffixes, ArgCount),
     length(Args, ArgCount)
       -> assert_shadow_arguments(Args, Prefix, Suffixes, ID),
          default_asserted_id(Prefix, ID)
    ;
     throw('tile shadow argument count different from count of shadow argument tile_predicates')
    ).

assert_shadow_arguments([], _, [], _).
assert_shadow_arguments([H|T], Prefix, [HP|TP], ID) :-
    assert_shadow_argument(H, Prefix, HP, ID),
    assert_shadow_arguments(T, Prefix, TP, ID).

assert_shadow_argument(Arg, Prefix, Suffix, ID) :-
    construct_data_predicate(Prefix, Suffix, Predicate),
    GoalR =.. [Predicate, ID, _],
    Goal =.. [Predicate, ID, Arg],
    retractall(GoalR),
    assertz(Goal).

default_asserted_id(Prefix, ID) :-
    atom_concat(Prefix, '_default_id', Predicate),
    GoalR =.. [Predicate, _],
    Goal =.. [Predicate, ID],
    retractall(GoalR),
    assertz(Goal).

data_predicate_dynamics :-
    findall(Prefix-Suffixes, data_predicates(_, Prefix, Suffixes), All),
    data_predicate_dynamics(All).

data_predicate_dynamics([]).
data_predicate_dynamics([Prefix-Suffixes|T]) :-
    data_predicate_dynamics(Suffixes, Prefix),
    atom_concat(Prefix, '_default_id', DefaultPredicate),
    (dynamic(DefaultPredicate / 1)),
    data_predicate_dynamics(T).

data_predicate_dynamics([], _).
data_predicate_dynamics([H|T], Prefix) :-
    data_predicate_dynamic(H, Prefix),
    data_predicate_dynamics(T, Prefix).

data_predicate_dynamic(Suffix, Prefix) :-
    construct_data_predicate(Prefix, Suffix, Predicate),
    (dynamic(Predicate / 2)), % BUG: 'dynamic(Predicate/2), foo' is being parsed as dynamic (Predicate/2, foo).
    data_predicate_default_dynamic(Prefix, Predicate).

data_predicate_default_dynamic(Prefix, Predicate) :-
    (dynamic(Predicate /1)), % BUG: 'dynamic(Predicate/1), foo' is being parsed as dynamic (Predicate/1, foo).
    Head =.. [Predicate, Value],
    retractall((Head :- _)),
    atom_concat(Prefix, '_default_id', DefaultPredicate),
    DefaultGoal =.. [DefaultPredicate, DefaultID],
    BinaryGoal =.. [Predicate, DefaultID, Value],
    asserta((Head :- DefaultGoal, BinaryGoal)).

construct_data_predicate(Prefix, Suffix, Predicate) :-
    atom_concat(Prefix, '_', PrefixExtended),
    atom_concat(PrefixExtended, Suffix, Predicate).

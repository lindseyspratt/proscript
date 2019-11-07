:-module(wam_index_predicates, [wam_index_predicates/0]).

:- if(\+ (current_predicate(wam_compiler:current_compile_url/1), wam_compiler:current_compile_url(_))).
    :- use_module('../tools/wam_bootstrap').
    :- use_module('../tools/wam_bootstrap_util').
    :- use_module(url). % in proscriptls system.
    :- use_module(system_util). % in proscriptls system.
:- endif.

:- use_module(wam_assemble).

/*
Compiled predicates are currently represented as
{PredicateID : {is_public:Flag, clauses:{Key0 : {code: Codes, key:Key0}, Key1 : {code: Codes1, key:Key1}}, clause_keys:[Key0, Key1], next_key:Key2, key:PredicateID}, ...}
The KeyI is used like the label in the WAM to jump from code for one clause to the beginning of the code for another clause.
For instance an instruction such as 'try_me_else 1' indicates that the code to jump to when the 'try_me_else' section fails
is the clause with index '1' (clauses are indexed starting at 0).

There is another interpretation of the label argument as an 'absolute' instruction offset in the current clause code.

To have indexing we need to have a hierarchy of labels for the sequences of clauses, not just per-clause labels.

Leave the base clauses in place. Add a faux clause to manage the indexing after the base clauses and add a start_key
member to guide use of the predicate, whether to jump to the index pseudo-clause or the base clause 0.

Example:

Program:
baz(f(a)) :- s.
baz([a]) :- t.
baz(X) :- m.
baz(b) :- n.
baz(f(b)) :- sb.
baz(f(c)) :- sc.

index pseudo-clause:

abstract instructions:
[
% Sequence 1
label(_46724),
try_me_else(_46756),
switch_on_term(0,fail,clause_offset(1),_46746),
label(_46746),
switch_on_structure(1,[0-clause_offset(0)]),

% Sequence 2
label(_46756),
retry_me_else(_47082),
goto_clause(2),

% Sequence 3
label(_47082),
trust_me,
switch_on_term(3,_47110,fail,_47114),
label(_47110),
switch_on_constant(1,[14-clause_offset(3)]),
label(_47114),
switch_on_structure(1,[0-_47698]),
label(_47698),
try(4),
trust(5)
]

labelled instruction words:

% 0x80000000 = 2147483648
% 2147483659 - 0x80000000 = 11

[
% Sequence 1
% try_me_else(_46756)
0-28,
1-2147483659, -> offset 11

% switch_on_term(0,fail,clause_offset(1),_46746)
2-44,
3-0,
4-0,
5-1,
6-2147483655, -> offset 7

% switch_on_structure(1,[0-clause_offset(0)]),
7-46,
8-1,
9-0,
10-0,

% Sequence 2
% retry_me_else(_47082),
11-29,
12-2147483663, % -> offset 15

% goto_clause(2),
13-74,
14-2,

% Sequence 3
% trust_me,
15-30,
16-0,

% switch_on_term(3,_47110,fail,_47114),
17-44,
18-3,
19-2147483670, % -> offset 22
20-0,
21-2147483674, % -> offset 26

% switch_on_constant(1,[14-clause_offset(3)]),
22-45,
23-1,
24-14,
25-3,

% switch_on_structure(1,[0-_47698]),
26-46,
27-1,
28-0,
29-2147483678, % -> offset 30

% try(4),
30-71,
31-4,

% trust(5)
32-73,
33-5]

*/

%wam_index_writeln(Term) :-
%    writeln(Term).
wam_index_writeln(_).

wam_index_predicates :-
    (\+ indexing_mode(none)
      -> indexable_compiled_predicates(Ps),
         wam_index_predicates(Ps)
    ;
    true
    ).

wam_index_predicates([]).
wam_index_predicates([H|T]) :-
    wam_index_predicate(H),
    wam_index_predicates(T).

% {173: {is_public:false, clauses:{0:{code:[254,0,4,172], key:0}}, clause_keys:[0], next_key:1, key:173}, ... }
% clause_table(PredicateID, ClauseOffset, ClauseCodes, Head, Body)
wam_index_predicate(PredicateID) :-
    lookup_functor(Functor, Arity, PredicateID),
    wam_index_writeln('==='),
    wam_index_writeln(indexing(Functor/Arity)),
    compiled_clauses(PredicateID, Clauses),
    wam_index_clauses(Clauses, PredicateID).

% wam_index_clauses(Clauses, PredicateID)
wam_index_clauses([], _PredicateID) :- !.
wam_index_clauses([_], PredicateID) :- !, register_indexed_predicate(PredicateID).
wam_index_clauses([H1, H2|T], PredicateID) :-
    wam_index_clauses1([H1, H2|T], PredicateID),
    register_indexed_predicate(PredicateID).

wam_index_clauses1(Clauses, PredicateID) :-
    Sequences = [Sequence|SequencesTail],
    clause_sequences(Clauses, Sequence, SequencesTail),
    trim_sequences(Sequences, TrimmedSequences),
    (degenerate_sequences(TrimmedSequences)
        -> wam_index_writeln(degenerate(TrimmedSequences))
    ;
    index_sequences(TrimmedSequences, _, IndexedSequences),
    wam_index_writeln(IndexedSequences),
    reset_compile_buffer,
    assemble(IndexedSequences, 0),
    compile_buffer_codes(Codes),
    wam_index_writeln(Codes),
    sequences_ids(TrimmedSequences, SequenceIDs),
    edit_clauses_for_index_sequences(SequenceIDs, PredicateID),
    add_index_clause_to_predicate(PredicateID)
    ).

degenerate_sequences([]).
degenerate_sequences([[_]|T]) :-
    degenerate_sequences(T).

sequences_ids([], []).
sequences_ids([H|T], [HI|TI]) :-
    sequence_ids(H, HI),
    sequences_ids(T, TI).

sequence_ids([], []).
sequence_ids([ID-_|T], [ID|TIDs]) :-
    sequence_ids(T, TIDs).


% clause_sequences(Clauses, Sequences).
% Find Sequences in Clauses [S1, S2|_] such that all of the clauses in
% Si have the first argument of the head bound OR the sequence is a single clause and the
% first argument of the head is a variable.
clause_sequences([], [], []).
clause_sequences([H|T], Sequence, OtherSequences) :-
    clause_sequence(H, Sequence, SequenceTail, OtherSequences, OtherSequencesTail),
    clause_sequences(T, SequenceTail, OtherSequencesTail).

% Clause has var in first arg so it belongs on its own in a sequence.
% Start a new sequence and add it to the list of Sequences.
clause_sequence(Clause, [], NewSequence, [[Clause], NewSequence|SequencesTail], SequencesTail) :-
    head_has_var_in_first_arg(Clause),
    !.
% Clause does not have a var in first arg so it is added to the current sequence.
% Add it to the current sequence and leave the overall list of Sequences unchanged.
clause_sequence(Clause, [Clause|SequenceTail], SequenceTail, Sequences, Sequences) :-
    \+ head_has_var_in_first_arg(Clause).

% trim_sequences(Sequences, TrimmedSequences).
% Empty sequences are introduced by consecutive var-in-first-arg clauses.
% Remove the empty sequences. E.g. [ [a], [], [b] ] => [ [a], [b] ].
trim_sequences([], []).
trim_sequences([H|T], Trimmed) :-
    trim_sequence(H, Trimmed, TrimmedTail),
    trim_sequences(T, TrimmedTail).

trim_sequence([], Trimmed, Trimmed).
trim_sequence([H|T], [[H|T]|TrimmedTail], TrimmedTail).

% index_sequences(Sequences, IndexedSequences)
% For [S1, S2, S3]:
% [ [try_me_else s2] | S1 |  [s2: retry_me_else s3] | S2 | [s3: trust_me] | S3]
% The first sequence (S1) starts with try_me_else pointing at S2, then retry_me_else pointing at S3, then trust_me:
%
%   keyS1:  try_me_else keyS2
%           {S1 code}
%
%   keyS2:  retry_me_else keyS3
%           {S2 code}
%
%   keyS3:  trust_me
%           {S3 code}
%
% Within the code for a sequence the general layout is:
%           switch_on_term VarKey, ConstAddress, ListAddress, StrucAddress
%   ConstAddress:   switch_on_constant Nc, {C1 : keyC1+, C2 : keyC2+, ..., CNc : keyCNc+}
%   ListAddress:    switch_on_list ...
%   StrucAddress:   switch_on_structure Ns, {P1 : keyP1+, ... , PNs : keyPNs+}
%
% The VarKey label is always the first proper clause (Key0).
% The targets of the switch_on_(constant list structure) instructions can either be
% addresses (within the sequence 'header' code) or base clause indexes.
% If a base clause index is used then the targeted instruction is the instruction
% *after* the first instruction of the clause. The first instruction of a clause
% is always NOP, try_me_else, retry_me_else, or trust_me. These are skipped when
% executing from the 'second level' switch instructions (i.e. not the switch_on_term instruction).
%
% If there is one clause with the specified constant/list/functor-arity, then
% the target is the key/index for that base clause (plus 1 instruction).
% Otherwise the sequence contains a try/retry/trust sequence where each of these
% instructions targets a base clause (plus 1 instruction).

index_sequences([], _, []).
index_sequences([[H]|T], SeqLabel, [label(SeqLabel), Instruction, goto_clause(Offset)|TI]) :-
    head_has_var_in_first_arg(H),
    !,
    H = Offset-_,
    (T = [_|_]
       -> Instruction = try_me_else(NextSeqLabel)
     ;
     Instruction = nop2
    ),
    index_sequences1(T, NextSeqLabel, TI).
index_sequences([H|T], SeqLabel, [label(SeqLabel), Instruction, switch_on_term(V, CA, CI, CF, L, S)|HI]) :-
    (T = [_|_]
       -> Instruction = try_me_else(NextSeqLabel)
     ;
     Instruction = nop2
    ),
    index_sequence(H, V, CA, CI, CF, L, S, HI, TI),
    index_sequences1(T, NextSeqLabel, TI).

index_sequences1([], _, []).
index_sequences1([[H]|T], SeqLabel, [label(SeqLabel), Instruction, goto_clause(Offset)|TI]) :-
    head_has_var_in_first_arg(H),
    !,
     H = Offset-_,
    (T = [_|_]
       -> Instruction = retry_me_else(NextSeqLabel)
     ;
     Instruction = trust_me
    ),
    index_sequences1(T, NextSeqLabel, TI).
index_sequences1([H|T], SeqLabel, [label(SeqLabel), Instruction, switch_on_term(V, CA, CI, CF, L, S)|HI]) :-
    (T = [_|_]
       -> Instruction = retry_me_else(NextSeqLabel)
     ;
     Instruction = trust_me
    ),
    index_sequence(H, V, CA, CI, CF, L, S, HI, TI),
    index_sequences1(T, NextSeqLabel, TI).

index_sequence([H|T], ClauseOffset, CA, CI, CF, L, S, SeqIndexed, SeqIndexedTailFinal) :-
    H = ClauseOffset-_,
    analyze([H|T], Atoms, Integers, Floats, Lists, Structures),

    group(Atoms, atom, GroupedAtoms,                GroupInstructions,  GroupTail1),
    group(Integers, integer, GroupedIntegers,       GroupTail1,         GroupTail2),
    group(Floats, float, GroupedFloats,             GroupTail2,         GroupTail3),
    group(Lists, list, [L],                         GroupTail3,         GroupTail4),
    group(Structures, structure, GroupedStructures, GroupTail4,         SeqIndexedTailFinal),

    switch_instruction(GroupedAtoms, constant, CA, SeqIndexed, SeqIndexedTail1),
    switch_instruction(GroupedIntegers, constant, CI, SeqIndexedTail1, SeqIndexedTail2),
    switch_instruction(GroupedFloats, constant, CF, SeqIndexedTail2, SeqIndexedTail3),
    switch_instruction(GroupedStructures, structure, S, SeqIndexedTail3, GroupInstructions).

analyze([], [], [], [], [], []).
analyze([H|T], [H|Atoms], Integers, Floats, Lists, Structures) :-
    head_has_atom_in_first_arg(H),
    !,
    analyze(T, Atoms, Integers, Floats, Lists, Structures).
analyze([H|T], Atoms, [H|Integers], Floats, Lists, Structures) :-
    head_has_integer_in_first_arg(H),
    !,
    analyze(T, Atoms, Integers, Floats, Lists, Structures).
analyze([H|T], Atoms, Integers, [H|Floats], Lists, Structures) :-
    head_has_float_in_first_arg(H),
    !,
    analyze(T, Atoms, Integers, Floats, Lists, Structures).
analyze([H|T], Atoms, Integers, Floats, [H|Lists], Structures) :-
    head_has_list_in_first_arg(H),
    !,
    analyze(T, Atoms, Integers, Floats, Lists, Structures).
analyze([H|T], Atoms, Integers, Floats, Lists, [H|Structures]) :-
    head_has_structure_in_first_arg(H),
    !,
    analyze(T, Atoms, Integers, Floats, Lists, Structures).

% group(ClauseInfos, Type, GroupedTypes, GroupInstructions, GroupTail).
group(ClauseInfos, list, GroupedConstants, GroupInstructions, GroupTail) :-
    !,
    list_group_instruction(ClauseInfos, GroupedConstants, GroupInstructions, GroupTail).
group(ClauseInfos, Type, GroupedConstants, GroupInstructions, GroupTail) :-
    ((Type = atom; Type = integer; Type = float)
      -> constants(ClauseInfos, ConstantClauses)
    ;
     Type = structure
      -> structures(ClauseInfos, ConstantClauses)
    ),
    (
    group1(ConstantClauses, [], Groups),
    sort(Groups, SortedGroups)
    %setof(Value-Clauses, setof(Clause, member(Value-Clause, ConstantClauses), Clauses), SortedGroups)
      -> group_instructions(SortedGroups, GroupedConstants, GroupInstructions, GroupTail)
    ;
     GroupInstructions = GroupTail
    ).

group1([], VG, VG).
group1([Value-Info|T], VGIn, VGOut) :-
    group1(Value, Info, VGIn, VGNext),
    group1(T, VGNext, VGOut).

% group1(Value, Info, ValueGroups, VGTail).
group1(Value, Info, VGIn, VGOut) :-
    select(Value-Bucket, VGIn, Value-NewBucket, VGOut)
      -> sort([Info|Bucket], NewBucket)
    ;
    VGOut = [Value-[Info]|VGIn].

% select/4 is from library/listut.pl
select(X, [X|Tail], Y, [Y|Tail]).
select(X, [Head|Xlist], Y, [Head|Ylist]) :-
	select(X, Xlist, Y, Ylist).

constants([], []).
constants([H|T], [V-H|TC]) :-
    H = _-Head/_Codes,
    head_constant_in_first_arg(Head, V),
    constants(T, TC).

structures([], []).
structures([H|T], [V-H|TC]) :-
    H = _-Head/_Codes,
    head_structure_in_first_arg(Head, V),
    structures(T, TC).

list_group_instruction([], [fail], GroupInstructions, GroupInstructions) :- !.
list_group_instruction([Offset-_], [clause_offset(Offset)], GroupInstructions, GroupInstructions) :- !.
list_group_instruction([C1,C2|CT], [GroupLabel], [label(GroupLabel)|GroupInstructions], GroupTail) :-
    % try C1, retry C2, ... , trust Cn
    group_instruction1([C1,C2|CT], GroupInstructions, GroupTail).

% group_instructions(Groups, GroupedConstants, GroupInstructions, GroupTail)
group_instructions([], [], GroupInstructions, GroupInstructions).
group_instructions([H|T], [HG|TG], GroupInstructions, GroupTail) :-
    group_instruction(H, HG, GroupInstructions, GroupNext),
    group_instructions(T, TG, GroupNext, GroupTail).

group_instruction(Value-[Offset-_], Value-clause_offset(Offset), GroupInstructions, GroupInstructions) :- !.
group_instruction(Value-[C1,C2|CT], Value-GroupLabel, [label(GroupLabel)|GroupInstructions], GroupTail) :-
    % try C1, retry C2, ... , trust Cn
    group_instruction1([C1,C2|CT], GroupInstructions, GroupTail).

group_instruction1([Offset - _,C2|CT], [try(Offset)|GroupNext], GroupTail) :-
    !,
    group_instruction2([C2|CT], GroupNext, GroupTail).
group_instruction1([Offset-_], [trust(Offset)|GroupTail], GroupTail).

group_instruction2([Offset-_, C2|CT], [retry(Offset)|GroupNext], GroupTail) :-
    !,
    group_instruction2([C2|CT], GroupNext, GroupTail).
group_instruction2([Offset-_], [trust(Offset)|GroupTail], GroupTail).

switch_instruction([], _, fail, SeqIndexed, SeqIndexed) :- !.
switch_instruction(GroupedTerms, Type, Label, [label(Label), Instruction|SeqIndexedTail], SeqIndexedTail) :-
    length(GroupedTerms, N),
    type_instruction(Type, N, GroupedTerms, Instruction).

type_instruction(constant, N, GroupedTerms, switch_on_constant(N, SortedGroupedTerms)) :- sort(GroupedTerms, SortedGroupedTerms).
type_instruction(structure, N, GroupedTerms, switch_on_structure(N, SortedGroupedTerms)) :- sort(GroupedTerms, SortedGroupedTerms).

head_has_var_in_first_arg(_ClauseOffset-Head/_Codes) :-
    arg(1, Head, Arg),
    var(Arg).

head_has_atom_in_first_arg(_ClauseOffset-Head/_Codes) :-
    arg(1, Head, Arg),
    atom(Arg).

head_has_integer_in_first_arg(_ClauseOffset-Head/_Codes) :-
    arg(1, Head, Arg),
    integer(Arg).

head_has_float_in_first_arg(_ClauseOffset-Head/_Codes) :-
    arg(1, Head, Arg),
    float(Arg).

head_constant_in_first_arg(Head, ID) :-
    arg(1, Head, Arg),
    (atom(Arg)
     -> lookup_atom(Arg, ID)
    ;
    integer(Arg)
     -> ID = Arg
    ;
    float(Arg)
     -> lookup_float(Arg, ID)
    ;
    Arg == []
     -> Arg = ID
    ),
    !.

head_has_list_in_first_arg(_ClauseOffset-Head/_Codes) :-
    Head =.. [_Functor, [_|_]|_].

head_has_structure_in_first_arg(_ClauseOffset-Head/_Codes) :-
    head_structure_in_first_arg(Head, _).

head_structure_in_first_arg(Head, FunctorID) :-
    arg(1, Head, Structure),
    functor(Structure, Functor, Arity),
    lookup_functor(Functor, Arity, FunctorID).
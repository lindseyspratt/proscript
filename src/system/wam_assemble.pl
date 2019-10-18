% Assemble Opcodes into a state, beginning at memory address N
% The resulting bytes are emitted using emit_code(+Address, +Code)

:- module(wam_assemble, [assemble/2]).

:- if(\+ (current_predicate(wam_compiler:current_compile_url/1), wam_compiler:current_compile_url(_))).
    :- use_module('../tools/wam_bootstrap_util').
:- endif.

assemble(Opcodes, N):-
        term_variables(Opcodes, Variables),
        encode_opcodes_1(Opcodes, N, Labels, [], DanglingReferences, []),
        % Now build the jump table between clauses
        compile_message(linking(Variables, Labels)),
        link(Labels, DanglingReferences),
        compile_message(linked).

link([], _):- !.
link([label(Label, Address)|Labels], References):-
        link_1(Label, Address, References),
        link(Labels, References).

link_1(_Label, _Address, []):- !.
link_1(Label, Address, [address_of(L, N)|References]):-
        Label == L, !,
        compile_message(emitting_address_1_of(L, Address, to(N))),
        emit_code(N, Address),
        link_1(Label, Address, References).
link_1(Label, Address, [aux_address_of(L, N)|References]):-
        Label == L, !,
        compile_message(emitting_aux_address_2_of(L, Address, to(N))),
        A is Address xor 0x80000000,
        emit_code(N, A),
        link_1(Label, Address, References).
link_1(Label, Address, [_|References]):-
        link_1(Label, Address, References).


emit_codes(_, []):- !.
emit_codes(N, [Code|Codes]):-
        emit_code(N, Code),
        NN is N+1,
        emit_codes(NN, Codes).

encode_register(y(N), [0, N|T], T).
encode_register(x(N), [1, N|T], T).

encode_fail(1000000).

encode_opcodes_1([], _, L, L, D, D):- !.

encode_opcodes_1([aux_label(Label)|As], N, L, L2, D, D1):-
        !,
        add_clause_to_aux(Label, N, L, L1),
        compile_message(auxiliary(Label, N)),
        NN is N + 2,
        encode_opcodes_1(As, NN, L1, L2, D, D1).

encode_opcodes_1([call_aux(Label, Arity, Trim)|As], N, L, L1, [address_of(Label, Offset)|D], D1):-
        !,
        compile_message('    '(call_aux(Label, Arity, Trim, N))),
        Offset is N+1,
        emit_codes(N, [40]),
        NN is N+2,
        emit_codes(NN, [Arity, Trim]),
        compile_message(need_address_of(Label, at(Offset))),
        NNN is N+4,
        encode_opcodes_1(As, NNN, L, L1, D, D1).

encode_opcodes_1([execute_aux(Label, Arity)|As], N, L, L1, [address_of(Label, NN)|D], D1):-
        !,
        compile_message('    '(execute_aux(Label, Arity, N))),
        NN is N+1,
        emit_codes(N, [41]),
        N2 is N+2,
        emit_codes(N2, [Arity]),
        NNN is N+3,
        encode_opcodes_1(As, NNN, L, L1, D, D1).

encode_opcodes_1([try_me_else(Label)|As], N, L, L1, [aux_address_of(Label, NN)|D], D1):-
        !,
        % We do not necessarily know what Label is. Reserve a byte and add it to the list of things to resolve once compilation has completed.
        compile_message(try_me_else(Label, N)),
        NN is N+1,
        emit_codes(N, [28]),
        NNN is N+2,
        encode_opcodes_1(As, NNN, L, L1, D, D1).

encode_opcodes_1([retry_me_else(Label)|As], N, L, L1, [aux_address_of(Label, NN)|D], D1):-
        !,
        compile_message(retry_me_else(Label, N)),
        NN is N+1,
        emit_codes(N, [29]),
        NNN is N+2,
        encode_opcodes_1(As, NNN, L, L1, D, D1).

encode_opcodes_1([switch_on_term(VarClauseOffset, AtomLabelOrFail, IntegerLabelOrFail, FloatLabelOrFail, ListLabelOrFail, StructureLabelOrFail)|As],
            N, L, L1, D, D1):-
        !,
        compile_message(switch_on_term(VarClauseOffset, AtomLabelOrFail, IntegerLabelOrFail, FloatLabelOrFail, ListLabelOrFail, StructureLabelOrFail, N)),
        N1 is N+1,
        emit_codes(N, [44]),
        (VarClauseOffset = fail
          -> encode_fail(Fail),
             emit_code(N1, Fail) % 'fail' indicator in WAM switch_on_term implementation.
        ;
         emit_code(N1, VarClauseOffset)
        ),
        encode_label_or_fail(N1, N2, AtomLabelOrFail, D, DT1),
        encode_label_or_fail(N2, N3, IntegerLabelOrFail, DT1, DT2),
        encode_label_or_fail(N3, N4, FloatLabelOrFail, DT2, DT3),
        encode_label_or_fail(N4, N5, ListLabelOrFail, DT3, DT4),
        encode_label_or_fail(N5, N6, StructureLabelOrFail, DT4, DT5),
        N7 is N6 + 1,
        encode_opcodes_1(As, N7, L, L1, DT5, D1).

encode_opcodes_1([switch_on_constant(Count, Targets)|As],
            N, L, L1, D, D1):-
        !,
        compile_message(switch_on_constant(Count, Targets, N)),
        N1 is N+1,
        emit_codes(N, [45]),
        encode_switch_table_typed(Count, Targets, N1, NNext, L, Lx, D, Dx),
        % Nnext is N1 + 2*Count + 1,
        encode_opcodes_1(As, NNext, Lx, L1, Dx, D1).

encode_opcodes_1([switch_on_structure(Count, Targets)|As],
            N, L, L1, D, D1):-
        !,
        compile_message(switch_on_structure(Count, Targets, N)),
        N1 is N+1,
        emit_codes(N, [46]),
        encode_switch_table_hash(Count, Targets, N1, NNext, L, Lx, D, Dx),
        % Nnext is N1 + 2*Count + 1,
        encode_opcodes_1(As, NNext, Lx, L1, Dx, D1).

encode_opcodes_1([label(Label)|As], N, [label(Label, N)|L], L1, D, D1):-
        !,
        encode_opcodes_1(As, N, L, L1, D, D1).

encode_opcodes_1([A|As], N, L, L1, D, D1):-
        !,
        compile_message('    '(A, N)),
        encode_opcode(A, OpcodeSize, Codes),
        emit_codes(N, Codes),
        NN is N + OpcodeSize,
        encode_opcodes_1(As, NN, L, L1, D, D1).

encode_label_or_fail(N, N1, LabelOrFail, D, DT) :-
        N1 is N+1,
        (var(LabelOrFail)
          -> D = [aux_address_of(LabelOrFail, N1)|DT]
        ; LabelOrFail = fail
          -> encode_fail(Fail),
             emit_code(N1, Fail), % 'fail' indicator in WAM switch_on_term implementation.
             D = DT
        ; LabelOrFail = clause_offset(X)
          -> emit_code(N1, X),
             D = DT
        ;
         throw(invalid_address(LabelOrFail))
        ).

encode_switch_table_typed(Count, Targets, N, NOut, L, L1, D, D1) :-
        Count > 15
          -> emit_code(N, 1), % 1 is hash table type
             N1 is N + 1,
             encode_switch_table_hash(Count, Targets, N1, NOut, L, L1, D, D1)
        ;
        emit_code(N, 0), % 0 is key/value sequence table type
        N1 is N + 1,
        L = L1,
        encode_switch_table(Count, Targets, N1, NOut, D, D1).

% [BucketCount, B1, B2, ..., BCount, B1Size, label(B1), K1B1, V1B1, ..., KB1SizeB1, VB1SizeB1, label(B2), B2Size, K1B2, V1B2, ...]
% encode_switch_table_hash(3, [1-clause_offset(10), 2-clause_offset(20), 3-clause_offset(30)], 0, L, L1, D, D1).

encode_switch_table_hash(Count, Targets, N, NOut, L, L1, D, D1) :-
        N1 is N + 1,
        BucketPower is min(floor(log(Count) / log(2)) + 1, 8),
        BucketCount is 1 << BucketPower,
        emit_code(N, BucketCount),
        Mask is BucketCount - 1,
        label_targets(Targets, Mask, LabelledTargets),
        setof(BucketID-BucketTargets, setof(BucketTarget, member(BucketID-BucketTarget, LabelledTargets), BucketTargets), Buckets),
        complete_buckets(Buckets, BucketCount, CompletedBuckets),
        bucket_labels(CompletedBuckets, BucketLabels),
        encode_switch_bucket_labels(BucketLabels, N1, NX, D, DInterim),
        % NX is N1 + BucketCount + 1,
        encode_switch_bucket_tables(CompletedBuckets, BucketLabels, NX, NOut, L, L1, DInterim, D1).

label_targets([], _Mask, []).
label_targets([K-V|T], Mask, [BucketID-(K-V)|BT]) :-
        hash(K, Mask, BucketID),
        label_targets(T, Mask, BT).

hash(Key, Mask, BucketID) :-
        BucketID is (Key /\ Mask) + 1.

complete_buckets(Buckets, BucketCount, CompletedBuckets) :-
        complete_buckets(Buckets, 0, BucketCount, CompletedBuckets).

complete_buckets([], SoFar, BucketCount, CompletedBuckets) :-
        complete_buckets1(SoFar, BucketCount, CompletedBuckets).
complete_buckets([ID-Targets|T], SoFar, BucketCount, CompletedBuckets) :-
        Next is SoFar + 1,
        (ID = Next
          -> CompletedBuckets = [ID-Targets|CompletedBucketsTail],
             NextBuckets = T
        ;
        ID > Next
          -> CompletedBuckets = [Next-[]|CompletedBucketsTail],
             [ID-Targets|T] = NextBuckets
        ;
        ID < Next
          -> throw(invalid_bucket_id(ID))
        ),
        complete_buckets(NextBuckets, Next, BucketCount, CompletedBucketsTail).

complete_buckets1(BucketCount, BucketCount, []) :- !.
complete_buckets1(SoFar, BucketCount, [Next-[]|CompletedBucketsTail]) :-
        SoFar < BucketCount,
        Next is SoFar + 1,
        complete_buckets1(Next, BucketCount, CompletedBucketsTail).

bucket_labels([], []).
bucket_labels([_-Targets|T], [BH|BT]) :-
        (Targets = []
          -> BH = fail
        ;
        true
        ),
        bucket_labels(T, BT).

encode_switch_bucket_labels([], NOut, NOut, D, D).
encode_switch_bucket_labels([H|T], N, NOut, D, D1) :-
        (var(H)
          -> D = [aux_address_of(H, N)|DInterim]
        ;
        D = DInterim,
        encode_fail(Fail),
        emit_code(N, Fail) % 'fail' indicator in WAM switch_on_term implementation.
        ),
        N1 is N + 1,
        encode_switch_bucket_labels(T, N1, NOut, DInterim, D1).

encode_switch_bucket_tables([], [], NOut, NOut, L, L, D, D).
encode_switch_bucket_tables([_-BucketTargets|T], [LH|LT], N, NOut, [label(LH, N)|L], L1, D, D1) :-
        var(LH),
        !,
        length(BucketTargets, BucketTargetCount),
        encode_switch_table(BucketTargetCount, BucketTargets, N, NInterim, D, DInterim),
        % NX is N + 2 * BucketTargetCount + 1,
        encode_switch_bucket_tables(T, LT, NInterim, NOut ,L, L1, DInterim, D1).
encode_switch_bucket_tables([_|T], [LH|LT], N, NOut, L, L1, D, D1) :-
        LH == fail,
        !,
        encode_switch_bucket_tables(T, LT, N, NOut, L, L1, D, D1).

encode_switch_table(Count, Targets, N, NOut, D, D1) :-
        emit_code(N, Count),
        N1 is N + 1,
        encode_switch_table_targets(Targets, N1, NOut, D, D1).

encode_switch_table_targets([], NOut, NOut, D, D).
encode_switch_table_targets([H|T], N, NOut, D, D1) :-
        encode_switch_table_target(H, N, NNext, D, DInterim),
        encode_switch_table_targets(T, NNext, NOut, DInterim, D1).

% codes: K, v
% where V is a constant (from a clause_offset) or it is a label
% for a sequence address to be resolved later.
% K (the key) is an encoding of some Prolog term (generally a
% atom or a predicate (functor/arity)).

encode_switch_table_target(K - V, N, NOut, D, D1) :-
        emit_code(N, K),
        N1 is N + 1,
        NOut is N1 + 1,
        (var(V) % wait to emit code when label V is resolved by link/2 predicate.
          -> D = [aux_address_of(V, N1)|D1]
        ;
         V = clause_offset(VC)
          -> emit_code(N1, VC),
             D = D1
        ;
         throw(invalid_table_value(K, V))
        ).

% This is used ONLY for printing out partially-compiled instructions
encode_opcode(clause(_), 0, []).

/* Control instructions 1-5 */
encode_opcode(allocate, 1, [1]).
encode_opcode(deallocate, 1, [2]).
encode_opcode(call(Functor/Arity, N), 3, [3, I, N]):-
        lookup_functor(Functor, Arity, I).
encode_opcode(execute(Functor/Arity), 2, [4, I]):-
        lookup_functor(Functor, Arity, I).
encode_opcode(proceed, 1, [5]).

/* Put instructions 6-14, 51, 60 */
encode_opcode(put_variable(y(N), x(I)), 3, [6, N, I]).
encode_opcode(put_variable(y(N)), 2, [60, N]).
encode_opcode(put_variable(x(N), x(I)), 3, [7, N, I]).
encode_opcode(put_value(R, x(I)), 4, [8|S]):-
        encode_register(R, S, [I]).
encode_opcode(put_unsafe_value(y(N), x(I)), 3, [9, N, I]).
encode_opcode(put_unsafe_value(x(N), x(I)), 3, [9, N, I]):-
        throw(illegal_unsafe_value_register(x(N))).
encode_opcode(put_constant(C, x(I)), 3, [10, K, I]):-
        lookup_atom(C, K).
encode_opcode(put_nil(x(I)), 2, [11,I]).
encode_opcode(put_structure(Functor/Arity, x(I)), 3, [12, F, I]):-
        lookup_functor(Functor, Arity, F).
encode_opcode(put_list(x(I)), 2, [13, I]).
encode_opcode(put_integer(C, x(I)), 3, [14, C, I]).
encode_opcode(put_float(C, x(I)), 3, [51, N, I]):-
        lookup_float(C, N).

/* Get instructions 15-21, 50 */
encode_opcode(get_variable(R, x(I)), 4, [15|S]):-
        encode_register(R, S, [I]).
encode_opcode(get_value(R, x(I)), 4, [16|S]):-
        encode_register(R, S, [I]).
encode_opcode(get_constant(C, x(I)), 3, [17, K, I]):-
        lookup_atom(C, K).
encode_opcode(get_nil(x(I)), 2, [18, I]).
encode_opcode(get_structure(Functor/Arity, x(I)), 3, [19, F, I]):-
        lookup_functor(Functor, Arity, F).
encode_opcode(get_list(x(I)), 2, [20, I]).
encode_opcode(get_integer(C, x(I)), 3, [21, C, I]).
encode_opcode(get_float(C, x(I)), 3, [50, N, I]):-
        lookup_float(C, N).

/* Unify instructions 22-27, 52 */
encode_opcode(unify_void(N), 2, [22, N]).
encode_opcode(unify_variable(R), 3, [23|S]):-
        encode_register(R, S, []).
encode_opcode(unify_value(R), 3, [24|S]):-
        encode_register(R, S, []).
encode_opcode(unify_local_value(R), 3, [25|S]):-
        encode_register(R, S, []).
encode_opcode(unify_constant(C), 2, [26, K]):-
        lookup_atom(C, K).
encode_opcode(unify_integer(C), 2, [27, C]).
encode_opcode(unify_float(C), 2, [52, N]):-
        lookup_float(C, N).


/* Indexing instructions 28-30, 71-73 */

encode_opcode(try_me_else(L), 2, [28, L]).
encode_opcode(retry_me_else(L), 2, [29, L]).
encode_opcode(trust_me, 2, [30, 0]).
encode_opcode(switch_on_term(V,C,L,S), 5, [44, V, C, L, S]).
encode_opcode(switch_on_constant(N,T), Size, [45, N|T]) :- Size is 2 + 2 * N.
encode_opcode(switch_on_structure(N,T), Size, [46, N|T]) :- Size is 2 + 2 * N.
encode_opcode(try(L), 2, [71, L]).
encode_opcode(retry(L), 2, [72, L]).
encode_opcode(trust(L), 2, [73, L]).
encode_opcode(goto_clause(L), 2, [74, L]).

/* Cut instructions */
encode_opcode(neck_cut, 1, [31]).
encode_opcode(cut(y(I)), 2, [32, I]).
encode_opcode(get_level(y(I)), 2, [33, I]).

/* Aux instructions. Used for ; and ->. Basically just call with an offset rather than a functor to look up*/
encode_opcode(call_aux(P, A, N), 4, [40, P, A, N]).
encode_opcode(execute_aux(P, A), 3, [41, P, A]).

/* retry_foreign is for foreign predicates with nondeterministic behaviour */
encode_opcode(retry_foreign, 1, [42]).

/* get_choicepoint is used for setup_call_cleanup */
encode_opcode(get_choicepoint(N, y(I)), 3, [43, N, I]).


encode_opcode(nop2, 2, [254, 0]).

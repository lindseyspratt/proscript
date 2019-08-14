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


encode_opcodes_1([try_me_else(Label)|As], N, L, L1, [address_of(Label, NN)|D], D1):-
        !,
        % We do not necessarily know what Label is. Reserve a byte and add it to the list of things to resolve once compilation has completed.
        compile_message(try_me_else(Label, N)),
        NN is N+1,
        emit_codes(N, [28]),
        NNN is N+2,
        encode_opcodes_1(As, NNN, L, L1, D, D1).

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

/* Put instructions 6-13 */
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

/* Get instructions 15-21 */
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

/* Unify instructions 22-27 */
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


/* Indexing instructions 28-30 */
encode_opcode(try_me_else(L), 2, [28, L]).
encode_opcode(retry_me_else(L), 2, [29, L]).
encode_opcode(trust_me, 2, [30, 0]).

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

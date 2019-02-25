trace :-
    '$trace_set'(trace).

notrace :-
    '$trace_set'(no_trace).


'$trace'(notrace) :-
    !,
    '$trace_set'(no_trace).

'$trace'(Goal) :-
    % Ensure trace_call == no_trace to prevent unbounded recursion.
    '$trace_set'(no_trace),

    '$trace_call_msg'(Goal),

    % Setting state.trace_call == 'trace_next_jmp' makes the $jmp predicate set state.trace_call == 'trace_next'.
    % (The $jmp predicate is used in the implementation of the call/1 predicate to prepare for the invocation of the
    % WAM call instruction.)
    % The 'trace_next' state makes the WAM call instruction set state.trace_call == 'trace'
    % so that the *next* evaluation of call instruction will invoke trace/1.

    '$trace_set'(trace_next_jmp),

    call(Goal),

    '$trace_set'(no_trace),
    '$trace_exit_msg'(Goal),
    '$trace_set'(trace).

'$trace_call_msg'(Goal) :- write('Call '), writeln(Goal).
'$trace_call_msg'(Goal) :- write('Fail '), writeln(Goal), !, fail.

'$trace_exit_msg'(Goal) :- write('Exit '), writeln(Goal).
'$trace_exit_msg'(Goal) :- write('Redo '), writeln(Goal), !, fail.


'$trace'(notrace, _) :-
    !,
    '$trace_set'(no_trace).

'$trace'(Goal, Ancestors) :-
    % Ensure trace_call == no_trace to prevent unbounded recursion.
    '$trace_set'(no_trace),

    '$trace_msg'('Call', 'Fail', Goal, Ancestors),

    '$trace_push_info'(Goal, Ancestors),

    % Setting state.trace_call == 'trace_next_jmp' makes the $jmp predicate set state.trace_call == 'trace_next'.
    % (The $jmp predicate is used in the implementation of the call/1 predicate to prepare for the invocation of the
    % WAM call instruction.)
    % The 'trace_next' state makes the WAM call instruction set state.trace_call == 'trace'
    % so that the *next* evaluation of call instruction will invoke trace/1.

    '$trace_set'(trace_next_jmp),

    call(Goal),

    '$trace_set'(no_trace),
    '$trace_set_info'(Ancestors),
    '$trace_msg'('Exit', 'Redo', Goal, Ancestors),
    '$trace_set'(trace).

'$trace_push_info'(Goal, Ancestors) :- '$trace_set_info'([Goal|Ancestors]).
'$trace_push_info'(_, Ancestors) :- '$trace_set_info'(Ancestors), !, fail.

'$trace_msg'(Success, _Failure, Goal, Ancestors) :- '$trace_msg1'(Success, Goal, Ancestors).
'$trace_msg'(_Success, Failure, Goal, Ancestors) :- '$trace_msg1'(Failure, Goal, Ancestors), !, fail.

'$trace_msg1'(Label, Goal, Ancestors) :- length(Ancestors, K), write(K), write(' '), write(Label), write(' '), writeln(Goal).


'$trace'(notrace, _, _) :-
    !,
    '$trace_set'(no_trace).

'$trace'(Goal, Ancestors, ID) :-
    % Ensure trace_call == no_trace to prevent unbounded recursion.
    '$trace_set'(no_trace),

    '$trace_msg'('Call', 'Fail', Goal, Ancestors, ID),

    '$trace_push_info'(Goal, Ancestors),

    % Setting state.trace_call == 'trace_next_jmp' makes the $jmp predicate set state.trace_call == 'trace_next'.
    % (The $jmp predicate is used in the implementation of the call/1 predicate to prepare for the invocation of the
    % WAM call instruction.)
    % The 'trace_next' state makes the WAM call instruction set state.trace_call == 'trace'
    % so that the *next* evaluation of call instruction will invoke trace/1.

    '$trace_set'(trace_next_jmp),

    call(Goal),

    '$trace_set'(no_trace),
    '$trace_set_info'(Ancestors),
    '$trace_msg'('Exit', 'Redo', Goal, Ancestors, ID),
    '$trace_set'(trace).

'$trace_msg'(Success, _Failure, Goal, Ancestors, ID) :- '$trace_msg1'(Success, Goal, Ancestors, ID).
'$trace_msg'(_Success, Failure, Goal, Ancestors, ID) :- '$trace_msg1'(Failure, Goal, Ancestors, ID), !, fail.

'$trace_msg1'(Label, Goal, Ancestors, ID) :-
    write(ID), write(' '), length(Ancestors, K), write(K),
    write(' '), write(Label), write(' '), writeln(Goal).


'$traceX'(notrace, _, _) :-
    !,
    '$trace_set'(no_trace).

'$traceX'(Goal, Ancestors, ID) :-
    % Ensure trace_call == no_trace to prevent unbounded recursion.
    '$trace_set'(no_trace),

    '$trace_retry',
    '$trace_interact'(call, fail, Goal, Ancestors, ID),

    call(Goal),

    '$trace_set'(no_trace),
    '$trace_set_info'(Ancestors),

    '$trace_retry',
    '$trace_interact'(exit, redo, Goal, Ancestors, ID).


'$trace_retry'.

'$trace_retry' :-
    '$trace_retry_value'(true),
    '$trace_set_retry'(false),
    '$trace_retry'.

'$trace_interact'(A, _B, G, Anc, ID) :- '$trace_interact'(A, G, Anc, ID).
'$trace_interact'(_A, B, G, Anc, ID) :- '$trace_retry_value'(false), '$trace_interact'(B, G, Anc, ID), !, fail.

'$trace_interact'(P, G, Anc, ID) :-
    '$trace_prompt'(P, G, Anc, ID), % set up a terminal prompt using state.trace_prompt
    '$trace_read_and_cmd'(P, G, Anc, ID).

 '$trace_read_and_cmd'(P, G, Anc, ID) :-
    repeat,
    read_char(X), % read a terminal command (using suspend)
    (member(X, [c, s, f, r, a])
    ;
    writeln('Commands are: "c" (creep), "s" (skip), "f" (fail), "r" (retry), or "a" (ancestors).'),
    fail),
    !, % this cut terminates the repeat/0.,
    '$trace_cmd'(X, P, G, Anc, ID).


'$trace_prompt'(Port, Goal, Ancestors, ID) :-
    length(Ancestors, K),
    pad_number(ID, 7, PaddedID),
    pad_number(K, 5, PaddedK),
    capitalize(Port, CapitalizedPort),
    concat_list([PaddedID, PaddedK, ' ', CapitalizedPort, ': ', Goal], Prompt),
    '$trace_set_prompt'(Prompt).

pad_number(N, Size, PaddedN) :-
    number_codes(N, NCodes),
    length(NCodes, NLength),
    (NLength < Size
      -> K is Size - NLength,
         pad_codes(K, PadCodes),
         append(PadCodes, NCodes, PaddedNCodes)
     ;
         append(" ", NCodes, PaddedNCodes)
    ),
    atom_codes(PaddedN, PaddedNCodes).

pad_codes(K, Codes) :-
    length(Codes, K),
    " " = [B],
    pad_codes1(Codes, B).


pad_codes1([], _).
pad_codes1([B|T], B) :-
    pad_codes1(T, B).

capitalize(A, CA) :-
    atom_codes(A, Xs),
    Xs = [X|Codes],
    capitalize_code(X, CX),
    atom_codes(CA, [CX|Codes]).

capitalize_code(X, CX) :-
    "a" = [AC],
    "z" = [ZC],
    (AC =< X, X =< ZC ->
      "A" = [AAC],
      K is X - AC,
      CX is K + AAC
     ;
     CX = X
    ).

'$trace_cmd'(c, call, G, Anc, _) :-
    !,
    '$trace_push_info'(G, Anc),

    % Setting state.trace_call == 'trace_next_jmp' makes the $jmp predicate set state.trace_call == 'trace_next'.
    % (The $jmp predicate is used in the implementation of the call/1 predicate to prepare for the invocation of the
    % WAM call instruction.)
    % The 'trace_next' state makes the WAM call instruction set state.trace_call == 'trace'
    % so that the *next* evaluation of call instruction will invoke trace/1.

    '$trace_set'(trace_next_jmp).

'$trace_cmd'(c, exit, _, _, _) :- % _L \= call
    !,
    '$trace_set'(trace).

'$trace_cmd'(c, _L, _, _, _) :- % _L \= call
    !.

'$trace_cmd'(s, call, _, _, _) :- % skip
    !,
    '$trace_set'(no_trace).

'$trace_cmd'(s, _, _, _, _) :- % skip
    !.

'$trace_cmd'(f, _, _, _, _) :- % fail
    !,
    fail.

'$trace_cmd'(r, _, _, _, _) :- % retry
    !,
    '$trace_set_retry'(true),
    fail.

'$trace_cmd'(a, P, G, Anc, ID) :- % ancestors
    !,
    write('Ancestors: '),
    writeln(Anc),
    '$trace_read_and_cmd'(P, G, Anc, ID).


read_char(X) :- get_terminal_char(X), !.
read_char(X) :- '$suspend', get_terminal_char(X).


'$suspend' :- '$suspend_set'(true), halt. % the evaluation is un-suspended using backtrack().
'$suspend' :- '$suspend_set'(false).

concat_list(L, A) :-
    concat_list(L, '', A).

concat_list([], A, A).
concat_list([H|T], A, B) :-
    (atom(H) ->
      atom_concat(A, H, X)
    ;
    number(H) ->
      number_codes(H, HC),
      atom_codes(HA, HC),
      atom_concat(A, HA, X)
    ;
    format(atom(HA), '~w', [H]),
    atom_concat(A, HA, X)
    ),
    concat_list(T, X, B).

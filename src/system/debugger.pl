:- module(debugger, [trace/0, notrace/0, notrace_backtrackable/0, '$traceR'/1]).

trace :-
    '$trace_set'(trace).

notrace :-
    '$trace_set'(no_trace).

notrace_backtrackable :-
    '$trace_set'(no_trace_backtrackable).

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

    '$trace_push_info'(ID, Goal, Ancestors),

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

'$trace_push_info'(ID, Goal, Ancestors) :- format(atom(X), '~w\n', [Goal]), '$trace_set_info'([ID-X|Ancestors]).
'$trace_push_info'(_, _, Ancestors) :- '$trace_set_info'(Ancestors), !, fail.

'$trace_msg'(Success, _Failure, Goal, Ancestors, ID) :- '$trace_msg1'(Success, Goal, Ancestors, ID).
'$trace_msg'(_Success, Failure, Goal, Ancestors, ID) :- '$trace_msg1'(Failure, Goal, Ancestors, ID), !, fail.

'$trace_msg1'(Label, Goal, Ancestors, ID) :-
    write(ID), write(' '), length(Ancestors, K), write(K),
    write(' '), write(Label), write(' '), writeln(Goal).

% === Retry with Backtrack Frame ===

'$traceR'(notrace, _, _) :-
    !,
    '$trace_set'(no_trace).

'$traceR'(notrace_backtrackable, _, _) :-
    !,
    '$trace_set'(notrace_backtrackable).

'$traceR'(Goal, Ancestors, ID) :-

    %'$trace_is_suspended',
    '$trace_retry'(ID, B),
    '$trace_interact'(call, fail, Goal, Ancestors, ID, B),

    call(Goal),

    '$trace_suspend_if_active',

    ('$trace_is_suspended'
      -> '$trace_set_info'(Ancestors),
         '$trace_interact'(exit, redo, Goal, Ancestors, ID, B)
    ;
    true
    ).

% The suspend_leap_trace prevents the '$trace_retry'/2 and '$trace_interact'/6 goals from being traced at all
% but allows '$trace_interact' to apply leap/spy processing.
% The skip_trace also prevents these goals from being traced and tells '$trace_interact' to apply 'creep' processing
% (e.g. process all ports and goals) instead of leap/spy processing.

'$trace_is_suspended' :-
    '$trace_value'(Value),
    '$trace_is_suspended'(Value),
    !.

'$trace_is_suspended'(skip_trace).
'$trace_is_suspended'(suspend_leap_trace).

'$trace_suspend_if_active' :-
    '$trace_value'(Value),
    '$trace_suspend_if_active'(Value),
    !.

'$trace_suspend_if_active'(trace) :- !, '$trace_set'(skip_trace).
'$trace_suspend_if_active'(leap_trace) :- !, '$trace_set'(suspend_leap_trace).
'$trace_suspend_if_active'(_).


'$trace_retry'(ID, B) :-
    '$get_backtrack_frame'(B).

'$trace_retry'(ID, B) :-
    '$trace_retry_value'(ID),
    '$trace_set_retry'(none),
    '$trace_retry'(ID, B).


'$trace_interact'(A, _B, G, Anc, ID, Bk) :- '$trace_interact'(A, G, Anc, ID, Bk).
'$trace_interact'(_A, B, G, Anc, ID, Bk) :-
    \+ '$trace_value'(no_trace),
    \+ '$trace_value'(no_trace_backtrackable),
    '$trace_interact'(B, G, Anc, ID, Bk), !, fail.

'$trace_interact'(P, G, Anc, ID, B) :-
    '$trace_interaction_enabled'(P, G)
      -> '$trace_prompt'(P, G, Anc, ID), % set up a terminal prompt using state.trace_prompt
         '$trace_read_and_cmd'(P, G, Anc, ID, B)
    ;
    '$trace_cmd'(l, P, G, Anc, ID, B). % default to 'leap'


'$trace_interaction_enabled'(P, G) :-
    '$trace_spy_mode'(M),
    % write('Checking enabled for '), write(M), write(' '), write(P), write(' '), writeln(G),
    '$trace_interaction_enabled'(M, P, G).


'$trace_interaction_enabled'(all, _P, _G) :- !.

'$trace_interaction_enabled'(specified, P, G) :-
    !,
     % double-negative to avoid persistent bindings of variables in G (if any).
    \+ \+ (
      '$trace_spy_specification'(P, G, B),
      write('Found spec for '), write(P), write(' '), writeln(G),
      (B = true -> true ; call(B))
      ).


'$trace_spy_mode'(M) :-
    '$trace_value'(Value),
    '$trace_spy_mode1'(Value, M).

'$trace_spy_mode1'(trace, all).
'$trace_spy_mode1'(skip_trace, all).
'$trace_spy_mode1'(leap_trace, specified).
'$trace_spy_mode1'(suspend_leap_trace, specified).



% not supported: :- dynamic('$trace_spy_specification'/3).

'$trace_prompt'(Port, Goal, Ancestors, ID) :-
    length(Ancestors, K),
    '$trace_create_prompt'(K, Port, Goal, ID, Prompt),
    '$trace_set_prompt'(Prompt).


'$trace_create_prompt'(K, Goal, ID, Prompt) :-
    pad_number(ID, 7, PaddedID),
    pad_number(K, 5, PaddedK),
    concat_list([PaddedID, PaddedK, ' ', Goal], Prompt).

'$trace_create_prompt'(K, Port, Goal, ID, Prompt) :-
    pad_number(ID, 7, PaddedID),
    pad_number(K, 5, PaddedK),
    capitalize(Port, CapitalizedPort),
    concat_list([PaddedID, PaddedK, ' ', CapitalizedPort, ': ', Goal], Prompt).


'$trace_read_and_cmd'(P, G, Anc, ID, B) :-
    '$trace_check_command'(X),
    !,
    '$trace_cmd'(X, P, G, Anc, ID, B).


'$trace_check_command'(X) :-
    read_char(X),
    member(X, [c, s, l, (+), (-), f, r, g, a, n, m, x, y, z]),
    !.

'$trace_check_command'(X) :-
    writeln('Commands are: "c" (creep), "s" (skip), "l" (leap), "+" (spy this), "-" (nospy this), "f" (fail), "r" (retry), "g" (ancestors), "a" (abort), "n" (nodebug), "m" (creep wam), "x" (creep wam long), "y" (trace wam), "z" (trace wam long).'),
    '$trace_check_command'(X).


'$trace_cmd'(c, P, G, Anc, ID, B) :-
    !,
    '$trace_cmd_creep'(c, P, G, Anc, ID, B).

'$trace_cmd'(m, P, G, Anc, ID, B) :-
    !,
    '$trace_cmd_creep'(m, P, G, Anc, ID, B).

'$trace_cmd'(x, P, G, Anc, ID, B) :-
    !,
    '$trace_cmd_creep'(x, P, G, Anc, ID, B).

'$trace_cmd'(y, P, G, Anc, ID, _) :-
    !,
    '$trace_cmd_creep'(y, P, G, Anc, ID, B).

'$trace_cmd'(z, P, G, Anc, ID, B) :-
    !,
    '$trace_cmd_creep'(z, P, G, Anc, ID, B).

'$trace_cmd'(l, call, G, Anc, ID, _) :-
    !,
    '$trace_push_info'(ID, G, Anc),

    % Setting state.trace_call == 'trace_next_jmp' makes the $jmp predicate set state.trace_call == 'trace_next'.
    % (The $jmp predicate is used in the implementation of the call/1 predicate to prepare for the invocation of the
    % WAM call instruction.)
    % The 'trace_next' state makes the WAM call instruction set state.trace_call == 'trace'
    % so that the *next* evaluation of call instruction will invoke trace/1.

    '$trace_set'(leap_trace_next_jmp).

'$trace_cmd'(l, exit, _, _, _, _) :- % _L \= call
    !,
    '$trace_set'(leap_trace).

'$trace_cmd'(l, _L, _, _, _, _) :- % _L \= call and \= exit
    !.

'$trace_cmd'(s, call, _, _, _, _) :- % skip
    !,
    '$trace_set'(skip_trace).

'$trace_cmd'(s, _, _, _, _, _) :- % skip
    !.

'$trace_cmd'(n, _, _, _, _, _) :- % nodebug
    !,
    '$trace_set'(no_trace).

'$trace_cmd'(f, _, _, _, _, _) :- % fail
    !,
    fail.

'$trace_cmd'(r, _, _, _, ID, B) :- % retry
    !,
    '$trace_set_retry'(ID),
    '$set_backtrack_frame'(B),
    fail.

'$trace_cmd'(a, _, _, _, _, _) :- % abort
    !,
    halt.

'$trace_cmd'(g, P, G, Anc, ID, B) :- % ancestors
    !,
    '$trace_write_ancestors'(Anc),
    '$trace_read_and_cmd'(P, G, Anc, ID, B).

'$trace_cmd'(+,  P, G, Anc, ID, B) :- % add spy specification
    !,
    G =.. [F|As],
    length(As, L),
    length(Ts, L),
    GT =.. [F|Ts],
    assertz('$trace_spy_specification'(_, GT, true)),
    write('Spypoint placed on '), writeln(F / L),
    '$trace_read_and_cmd'(P, G, Anc, ID, B).

'$trace_cmd'(-,  P, G, Anc, ID, B) :- % remove all spy specifications for P/G (if any).
    !,
    G =.. [F|As],
    length(As, L),
    length(Ts, L),
    GT =.. [F|Ts],
    retractall('$trace_spy_specification'(_, GT, _)),
    write('Spypoint removed from '), writeln(F / L),
    '$trace_read_and_cmd'(P, G, Anc, ID, B).


'$trace_cmd_creep'(Cmd, call, G, Anc, ID, _) :-
    !,
    '$trace_push_info'(ID, G, Anc),

    % Setting state.trace_call == 'trace_next_jmp' makes the $jmp predicate set state.trace_call == 'trace_next'.
    % (The $jmp predicate is used in the implementation of the call/1 predicate to prepare for the invocation of the
    % WAM call instruction.)
    % The 'trace_next' state makes the WAM call instruction set state.trace_call == 'trace'
    % so that the *next* evaluation of call instruction will invoke trace/1.

    '$trace_cmd_creep_wam'(Cmd),
    '$trace_set'(trace_next_jmp).

'$trace_cmd_creep'(Cmd, exit, _, _, _, _) :- % _L \= call
    !,
    '$trace_set'(trace).

'$trace_cmd_creep'(Cmd, _L, _, _, _, _) :- % _L \= call and \= exit
    !.


'$trace_cmd_creep_wam'(c) :-
    '$trace_instruction_set'(no_trace),
    nodebug.

'$trace_cmd_creep_wam'(m) :-
    '$trace_instruction_set'(step),
    debug.

'$trace_cmd_creep_wam'(x) :-
    '$trace_instruction_set'(step),
    nodebug.

'$trace_cmd_creep_wam'(y) :-
    '$trace_instruction_set'(trace),
    debug.

'$trace_cmd_creep_wam'(z) :-
    '$trace_instruction_set'(trace),
    nodebug.


'$trace_write_ancestors'([]) :- !.
'$trace_write_ancestors'(Anc) :-
    reverse(Anc, RevAnc),
    writeln('Ancestors:'),
    '$trace_write_ancestors1'(RevAnc, 1).

'$trace_write_ancestors1'([],_).
'$trace_write_ancestors1'([ID-Goal|T], D) :-
    '$trace_create_prompt'(D, Goal, ID, Prompt),
    writeln(Prompt),
    DNext is D + 1,
    '$trace_write_ancestors1'(T, DNext).

read_char(X) :- get_terminal_char(X), !.
read_char(X) :- '$suspend', get_terminal_char(X).


'$suspend' :- '$suspend_set'(true), halt. % the evaluation is un-suspended using backtrack().
'$suspend' :- '$suspend_set'(false).


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


reverse(L, R) :-
    reverse(L, [], R).

reverse([], R, R).
reverse([H|T], X, R) :-
    reverse(T, [H|X], R).


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

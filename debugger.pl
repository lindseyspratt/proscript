% extend choicepoint with trace_call flag - restore value of flag on backtrack.

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
'$trace_exit_msg'(Goal) :- write('Retry '), writeln(Goal), !, fail.

/*
call((A, B), Mode) :-
    !,
    call(A, Mode),
    call(B, Mode).

call(G, trace) :-
    !,
    '$trace'(G).

call(G, trace_next_jmp) :-
    !,
    call1(B, trace).

call(true, _) :-
    !.

call(G, no_trace) :-
    call1(G, no_trace).


call1(G, Mode) :-
    clause(G, B),
    call(B, Mode).
*/


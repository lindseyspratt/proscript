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
    '$trace_exit_msg'(Goal).

'$trace_call_msg'(Goal) :- write('Call '), writeln(Goal).
'$trace_call_msg'(Goal) :- write('Fail '), writeln(Goal), !, fail.

'$trace_exit_msg'(Goal) :- write('Exit '), writeln(Goal).
'$trace_exit_msg'(Goal) :- write('Redo '), writeln(Goal), !, fail.

'$trace'(Goal, Ancestors) :-
    % Ensure trace_call == no_trace to prevent unbounded recursion.
    '$trace_set'(no_trace),

    '$trace_msg'('Call', 'Fail', Goal, Ancestors),

    '$trace_set_info'([Goal|Ancestors]),

    % Setting state.trace_call == 'trace_next_jmp' makes the $jmp predicate set state.trace_call == 'trace_next'.
    % (The $jmp predicate is used in the implementation of the call/1 predicate to prepare for the invocation of the
    % WAM call instruction.)
    % The 'trace_next' state makes the WAM call instruction set state.trace_call == 'trace'
    % so that the *next* evaluation of call instruction will invoke trace/1.

    '$trace_set'(trace_next_jmp),

    call(Goal),

    '$trace_set'(no_trace),
    '$trace_msg'('Exit', 'Redo', Goal, Ancestors).

'$trace_msg'(Success, _Failure, Goal, Ancestors) :- '$trace_msg1'(Success, Goal, Ancestors).
'$trace_msg'(_Success, Failure, Goal, Ancestors) :- '$trace_msg1'(Failure, Goal, Ancestors), !, fail.

'$trace_msg1'(Label, Goal, Ancestors) :- length(Ancestors, K), write(K), write(' '), write(Label), write(' '), writeln(Goal).

'$trace'(Goal, Ancestors, ID) :-
    % Ensure trace_call == no_trace to prevent unbounded recursion.
    '$trace_set'(no_trace),

    '$trace_msg'('Call', 'Fail', Goal, Ancestors, ID),

    '$trace_set_info'([Goal|Ancestors]),

    % Setting state.trace_call == 'trace_next_jmp' makes the $jmp predicate set state.trace_call == 'trace_next'.
    % (The $jmp predicate is used in the implementation of the call/1 predicate to prepare for the invocation of the
    % WAM call instruction.)
    % The 'trace_next' state makes the WAM call instruction set state.trace_call == 'trace'
    % so that the *next* evaluation of call instruction will invoke trace/1.

    '$trace_set'(trace_next_jmp),

    call(Goal),

    '$trace_set'(no_trace),
    '$trace_msg'('Exit', 'Redo', Goal, Ancestors, ID).

'$trace_msg'(Success, _Failure, Goal, Ancestors, ID) :- '$trace_msg1'(Success, Goal, Ancestors, ID).
'$trace_msg'(_Success, Failure, Goal, Ancestors, ID) :- '$trace_msg1'(Failure, Goal, Ancestors, ID), !, fail.

'$trace_msg1'(Label, Goal, Ancestors, ID) :-
    write(ID), write(' '), length(Ancestors, K), write(K),
    write(' '), write(Label), write(' '), writeln(Goal).

:- dynamic(current_report_group/1).
:- dynamic(test_counter/1).
:- dynamic(test_pass_count/1).
:- dynamic(test_fail_info/4).


run_tests :-
    reset_test_data,
    forall(clause(test(G,X,Y), _), run_test(test(G,X,Y))),
    report_test_summary.

reset_test_data :-
    retractall(current_report_group(_)),
    retractall(test_counter(_)),
    retractall(test_pass_count(_)),
    retractall(test_fail_info(_, _, _, _)).

report_test_summary :-
    (test_pass_count(PC)
     -> true
    ;
     PC = 0
    ),
    writeln(pass(PC)),
    findall(fail(ID, X, Expected, Result), test_fail_info(ID, X, Expected, Result), Failures),
    length(Failures, FC),
    writeln(fail(FC)),
    forall(member(fail(ID, X, Expected, Result), Failures), writeln(fail(ID, X, Expected, Result))).


run_test(test(G,X,Y)) :-
    catch(test(G, X, Y), B, true)
      -> (var(B)
           -> report_test(G, X, Y, succeeded)
         ;
          report_test(G, X, Y, B)
         )
    ;
    report_test(G, X, Y, failed).

report_test(G, X, Expected, Result) :-
    (retract(current_report_group(CG))
      -> (CG = G -> true;writeln(''), write('=='),writeln(G))
    ;
     write('=='),writeln(G)),
    asserta(current_report_group(G)),
    (retract(test_counter(C))
      -> NC is C + 1
     ;
     NC = 1
    ),
    asserta(test_counter(NC)),
    write(NC), write(': '),
    (Expected = Result
      -> writeln(pass(X, expected(Result))),
         update_test_pass_count
    ;
     writeln(fail(X, expected(Expected), result(Result))),
     update_test_fail_info(NC, X, Expected, Result)
    ).

update_test_pass_count :-
    (retract(test_pass_count(C))
      -> NC is C + 1
    ;
    NC = 1
    ),
    asserta(test_pass_count(NC)).

update_test_fail_info(ID, X, Expected, Result) :-
    assertz(test_fail_info(ID, X, Expected, Result)).

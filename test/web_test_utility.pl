:- use_module('../library/object').
:- dynamic(current_report_group/1).
:- dynamic(test_counter/1).
:- dynamic(test_pass_count/1).
:- dynamic(test_fail_info/4).
:- dynamic(test/3).


run_tests :-
    reset_test_data,
    findall(G-X, clause(test(G,X,_), _), Xs),
    length(Xs, XsLength),
    forall(clause(test(G,X,Y), _), run_test(test(G,X,Y), XsLength)),
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
    statistics,
    % "Pass " PC "Fail " FC
    number_codes(PC, PCs),
    number_codes(FC, FCs),
    append_lists(["Pass = ", PCs, ",<br>Fail = ", FCs], MsgCodes),
    MyBar >-> id :> msg,
    MyBar >+> innerHTML <: MsgCodes,
    (FC > 0
      -> (FC > 1
           -> Label = "<br>Failures:"
         ;
          Label = "<br>Failure:"
         ),
         MyProgress >-> id :> myProgress,
         MyProgress >*> insertAdjacentHTML(beforeEnd, Label),
         forall(member(fail(ID, X, Expected, Result), Failures),
            (number_codes(ID, IDCodes),
             atom_codes(X, XCodes),
             append_lists(["<br>", IDCodes, ": ", XCodes], FailCodes),
             MyProgress >*> insertAdjacentHTML(beforeEnd, FailCodes)))
    ;
     true
    ).



run_test(test(G,X,Y), TotalTestCount) :-
    catch(test(G, X, Y), B, true)
      -> (var(B)
           -> report_test(G, X, Y, succeeded, TotalTestCount)
         ;
          report_test(G, X, Y, B, TotalTestCount)
         )
    ;
    report_test(G, X, Y, failed, TotalTestCount).

report_test(G, X, Expected, Result, TotalTestCount) :-
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
    ),
    update_test_progress(NC, TotalTestCount).

update_test_pass_count :-
    (retract(test_pass_count(C))
      -> NC is C + 1
    ;
    NC = 1
    ),
    asserta(test_pass_count(NC)).

update_test_fail_info(ID, X, Expected, Result) :-
    assertz(test_fail_info(ID, X, Expected, Result)).

update_test_progress(TestSoFarCount, TotalTestCount) :-
    Percentage is 100 * TestSoFarCount / TotalTestCount,
    number_codes(Percentage, PercentageCodes),
    append(PercentageCodes, "%", PercentageStringCodes),
    atom_codes(PercentageString, PercentageStringCodes),
    MyBar >-> id :> myBar,
    MyBar >+> style :> Style,
    Style >*> setProperty(width, PercentageString),
    (test_fail_info(_, _, _, _)
      -> Style >*> setProperty('background-color', red)
    ;
     true
    ).
:- module(promise, [promise_result/2]).

:- if((current_predicate(wam_compiler:current_compile_url/1), current_compile_url(_))).
    :- use_module(bootstrap_js). % for halt/0
:- endif.

promise_result(Promise, _) :-
  request_result(Promise),
  halt.

promise_result(Promise, Result) :-
  handle_result(Promise, Result).

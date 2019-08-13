:- module(promise, [promise_result/2]).

promise_result(Promise, _) :-
  request_result(Promise),
  halt.

promise_result(Promise, Result) :-
  handle_result(Promise, Result).

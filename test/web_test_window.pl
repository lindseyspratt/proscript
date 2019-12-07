:- ensure_loaded(web_test_utility).
:- dynamic(test_result/1).

test('Window', 'localStorage', succeeded) :-
    retractall(test_result(_)),
    dom_window(Window),
    dom_object_property(_, Window, localStorage, S),
    dom_object_method(S, setItem(web_test_a, test_value_a)),
    dom_object_method(S, getItem(web_test_a, test_value_a)),
    dom_object_method(S, removeItem(web_test_a)),
    \+ dom_object_method(S, getItem(web_test_a, _)).

test('Window', 'sessionStorage', succeeded) :-
    retractall(test_result(_)),
    dom_window(Window),
    dom_object_property(_, Window, sessionStorage, S),
    dom_object_method(S, setItem(web_test_b, test_value_b)),
    dom_object_method(S, getItem(web_test_b, test_value_b)),
    dom_object_method(S, removeItem(web_test_b)),
    \+ dom_object_method(S, getItem(web_test_b, _)).

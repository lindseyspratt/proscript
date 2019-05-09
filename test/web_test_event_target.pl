:- ensure_loaded(web_test_utility).
:- dynamic(test_result/1).

test('EventTarget', 'eventListener add/dispatch', succeeded) :-
    retractall(test_result(_)),
    dom_element_attribute_value(E, id, test1),
    dom_object_method(E, addEventListener(test_event, asserta(test_result(test_event_dispatched)))),
    dom_object_method(E, dispatchEvent(test_event)),
    retract(test_result(test_event_dispatched)).

test('EventTarget', 'eventListener add/remove/dispatch', succeeded) :-
    retractall(test_result(_)),
    dom_element_attribute_value(E, id, test1),
    dom_object_method(E, addEventListener(test_event, asserta(test_result(test_event_dispatched)))),
    dom_object_method(E, removeEventListener(test_event, asserta(test_result(test_event_dispatched)))),
    dom_object_method(E, dispatchEvent(test_event)),
    \+ retract(test_result(test_event_dispatched)).

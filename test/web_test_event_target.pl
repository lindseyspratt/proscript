:- ensure_loaded(web_test_utility).
:- dynamic(test_result/1).

test('EventTarget', 'eventListener add/dispatch', succeeded) :-
    retractall(test_result(_)),
    dom_element_attribute_value(E, id, test1),
    Goal = [object-Event] ^ (dom_object_type(Event, Type), writeln(Type), asserta(test_result(test_event_dispatched))),
    dom_object_method(E, addEventListener(test_event1, Goal)),
    dom_object_method(E, dispatchEvent(test_event1)),
    dom_object_method(E, removeEventListener(test_event1, Goal)),
    retract(test_result(test_event_dispatched)).

test('EventTarget', 'eventListener add/remove/dispatch', succeeded) :-
    retractall(test_result(_)),
    dom_element_attribute_value(E, id, test1),
    dom_object_method(E, addEventListener(test_event2, asserta(test_result(test_event_dispatched)))),
    dom_object_method(E, removeEventListener(test_event2, asserta(test_result(test_event_dispatched)))),
    dom_object_method(E, dispatchEvent(test_event2)),
    \+ retract(test_result(test_event_dispatched)).

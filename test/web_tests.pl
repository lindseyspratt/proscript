:- dynamic(test_result/1).

event_listener_test :-
    dom_element_attribute_value(E, id, test1),
    dom_object_method(E, addEventListener(test_event, asserta(test_result(test_event_dispatched)))),
    dom_object_method(E, dispatchEvent(test_event)),
    !,
    (retract(test_result(test_event_dispatched))
      -> writeln(success)
     ;
     writeln(failed)
    ).

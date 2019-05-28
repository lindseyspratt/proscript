:- ensure_loaded(web_test_utility).

% Properties

test('HTMLTextAreaElement', 'autofocus of textarea', succeeded) :-
    dom_element_attribute_value(E, id, textarea),
    dom_object_property(_, E, autofocus,false),
    set_dom_object_property(E, autofocus,true),
    dom_object_property(_, E, autofocus,true),
    set_dom_object_property(E, autofocus,false).

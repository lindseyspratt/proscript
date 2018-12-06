simple_test :-
    dom_element_attribute_value(E, id, simpletest),
    set_dom_element_inner_html(E, "Hello World").

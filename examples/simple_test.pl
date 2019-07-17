simple_button_test :-
    dom_element_attribute_value(E, id, simpletest),
    create_dom_element('BUTTON', Button),
    set_dom_element_attribute_value(Button, class, 'example-button'),
    create_dom_text_node("Click Me", NewContent),
    append_dom_node_child(Button, NewContent),
    dom_object_method(Button, addEventListener(click, alert('Hello World!'))),
    append_dom_node_child(E, Button).

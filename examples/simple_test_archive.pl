
simple_test :-
    dom_element_attribute_value(E, id, simpletest),
    dom_object_property(_, E, clientHeight, H),
    number_codes(H, Hs),
    append("Hello World: ", Hs, Greeting),
    set_dom_object_property(E, innerHTML, Greeting).

simple_button_test :-
    dom_element_attribute_value(E, id, simpletest),
    create_dom_element('BUTTON', Button),
    set_dom_element_attribute_value(Button, class, 'example-button'),
    create_dom_text_node("Click Me", NewContent),
    append_dom_node_child(Button, NewContent),
    dom_object_method(Button, addEventListener(click, alert('Hello World!'))),
    append_dom_node_child(E, Button).

simple_button_test2 :-
    dom_element_attribute_value(E, id, simpletest),
    create_dom_element('BUTTON', Button),
    create_dom_text_node("Click Me", NewContent),
    append_dom_node_child(Button, NewContent),
    set_dom_element_attribute_value(Button, onclick, ''), % 'proscriptls(\'alert(\\\'Hello World!\\\')\')'),
    append_dom_node_child(E, Button).

add_element_test :-
  % create a new div element
  create_dom_element('DIV', NewDiv),
  % and give it some content
  create_dom_text_node("Hi there and greetings!", NewContent),
  % add the text node to the newly created div
  append_dom_node_child(NewDiv, NewContent),

  % add the newly created element and its content into the DOM
  dom_element_attribute_value(CurrentDiv, id, simpletest),
  dom_object_property(_, Body, tag, body),
  insert_before_dom_node(Body, CurrentDiv, NewDiv).

% === mem, p, q, and r predicates
% === for use in testing debugging trace feature.

mem(X, [X|_]).
mem(X, [_|T]) :- mem(X, T).

p :- q.
p :- r.

q.
r.

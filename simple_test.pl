simple_test :-
    dom_element_attribute_value(E, id, simpletest),
    dom_element_property(E, clientHeight, H),
    number_codes(H, Hs),
    append("Hello World: ", Hs, Greeting),
    set_dom_element_property(E, innerHTML, Greeting).

add_element_test :-
  % create a new div element
  create_dom_element('DIV', NewDiv),
  % and give it some content
  create_dom_text_node("Hi there and greetings!", NewContent),
  % add the text node to the newly created div
  append_dom_node_child(NewDiv, NewContent),

  % add the newly created element and its content into the DOM
  dom_element_attribute_value(CurrentDiv, id, simpletest),
  dom_element_property(Body, tag, body),
  insert_before_dom_node(Body, CurrentDiv, NewDiv).

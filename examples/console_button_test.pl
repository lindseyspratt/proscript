/*
The Console Button Test uses console_button_test.html and console_button_test.pl to create a
web page for testing a dynamic Prolog console.
There is initially only a single button name 'Create Console' that when clicked renames that
button 'Remove Console' and creates a prolog 'console' DIV on that page (if it does not already exist)
in which there is a 'stdout' DIV in which the user can run Prolog queries.
Clicking the 'Remove Console' button renames the button 'Create Console' and removes the 'stdout' DIV.
*/

:- initialization(console_button_test).

console_button_test :-
    dom_element_attribute_value(E, id, simpletest),
    create_dom_element('BUTTON', Button),
    set_dom_element_attribute_value(Button, id, console_button),
    set_dom_element_attribute_value(Button, class, 'example-button'),
    create_dom_text_node("Create Console", NewContent),
    append_dom_node_child(Button, NewContent),
    dom_object_method(Button, addEventListener(click, setup_div_and_console)),
    append_dom_node_child(E, Button).

setup_div_and_console :-
  dom_element_attribute_value(Button, id, console_button),
  dom_object_property(_, Button, innerText, Text),
  setup_div_and_console(Text, Button).

setup_div_and_console("Create Console", Button) :-
  set_dom_object_property(Button, innerText, "Remove Console"),
  setup_console_div,
  setup_console.

setup_div_and_console("Remove Console", Button) :-
  set_dom_object_property(Button, innerText, "Create Console"),
  remove_console_div.

% <div style="border: 1px solid black; height: 50%; width: 100%; overflow: scroll;" id="stdout" onKeyPress="keypress(event)" onKeyDown="keydown(event)" tabindex="0"></div>
setup_console_div :-
  create_dom_element('DIV', Div),
  set_dom_element_attribute_value(Div, style, 'border: 1px solid black; height: 50%; width: 100%; overflow: scroll;'),
  set_dom_element_attribute_value(Div, id, stdout),
  set_dom_element_attribute_value(Div, onkeypress, 'keypress(event)'),
  set_dom_element_attribute_value(Div, onkeydown, 'keydown(event)'),
%  dom_element_add_event_listener(Div, keypress, eval_javascript("keypress(event)")),
%  dom_element_add_event_listener(Div, keydown, eval_javascript("keydown(event)")),
  set_dom_element_attribute_value(Div, tabindex, '0'),
  lookup_console_div(OuterDiv),
  append_dom_node_child(OuterDiv, Div),
  dom_object_property(element, Body, tag, body),
  append_dom_node_child(Body, OuterDiv).

lookup_console_div(Div) :-
  dom_element_attribute_value(Div, id, console)
    -> true
  ;
  create_dom_element('DIV', Div),
  set_dom_element_attribute_value(Div, id, console).

remove_console_div :-
  dom_element_attribute_value(Div, id, console),
  set_dom_object_property(Div, innerText, "").

setup_console :-
  dom_object_property(element, Body, tag, body),
  set_dom_element_attribute_value(Body, onkeydown, 'return preventBackspace(event);'),
%  dom_element_add_event_listener(Body, keydown, eval_javascript("preventBackspace(event);")),
  eval_javascript("onload();").

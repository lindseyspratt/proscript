:- ensure_loaded(web_test_utility).

test('Node', 'childNode of test1 finds 3 children with types [node, htmlelement, node]', succeeded) :-
    dom_element_attribute_value(E, id, test1),
    findall(T,
                (dom_object_property(_, E, childNode,C),
                 dom_object_type(C, T)),
                [node, htmlelement, node]).

test('Node', 'firstChild of test1 finds child with type node', succeeded) :-
    dom_element_attribute_value(E, id, test1),
    dom_object_property(_, E, firstChild,C),
    dom_object_type(C, node).

test('Node', 'lastChild of test1 finds child with type node', succeeded) :-
    dom_element_attribute_value(E, id, test1),
    dom_object_property(_, E, lastChild,C),
    dom_object_type(C, node).

test('Node', 'nextSibling of test1 finds sibling node with type node', succeeded) :-
    dom_element_attribute_value(E, id, test1),
    dom_object_property(_, E, nextSibling,C),
    dom_object_type(C, node).

test('Node', 'nodeName of test1 finds DIV', succeeded) :-
    dom_element_attribute_value(E, id, test1),
    dom_object_property(_, E, nodeName,'DIV').

test('Node', 'nodeType of test1 finds 1', succeeded) :-
    dom_element_attribute_value(E, id, test1),
    dom_object_property(_, E, nodeType,1).

test('Node', 'nodeValue of test1 fails', failed) :-
    dom_element_attribute_value(E, id, test1),
    dom_object_property(_, E, nodeValue,_).

test('Node', 'ownerDocument of test1 finds node with type document', succeeded) :-
    dom_element_attribute_value(E, id, test1),
    dom_object_property(_, E, ownerDocument,D),
    dom_object_type(D, document).

test('Node', 'parentElement of test1 finds node with type htmlelement', succeeded) :-
    dom_element_attribute_value(E, id, test1),
    dom_object_property(_, E, parentElement,D),
    dom_object_type(D, htmlelement).

test('Node', 'parentNode of test1 finds node with type htmlelement', succeeded) :-
    dom_element_attribute_value(E, id, test1),
    dom_object_property(_, E, parentNode,D),
    dom_object_type(D, htmlelement).

test('Node', 'previousSibling of test1 finds sibling node with type node', succeeded) :-
    dom_element_attribute_value(E, id, test1),
    dom_object_property(_, E, previousSibling,C),
    dom_object_type(C, node).

test('Node', 'textContent of test1 finds text', succeeded) :-
    dom_element_attribute_value(E, id, test1),
    dom_object_property(_, E, textContent,[10,32,32,32,32,10,32,32,32,32,32,32,32,32,116,101,115,116,32,49,32,99,104,105,108,100,10,32,32,32,32,10]).

test('Node', 'cloneNode of test1 creates node with type htmlelement', succeeded) :-
    dom_element_attribute_value(E, id, test1),
    dom_object_method(E, cloneNode(false, C)),
    dom_object_type(C, htmlelement).

test('Node', 'compareDocumentPosition of test1 and firstChild gets position 20', succeeded) :-
    dom_element_attribute_value(E, id, test1),
    dom_object_property(_, E, firstChild,C),
    dom_object_method(E, compareDocumentPosition(C, 20)).

test('Node', 'contains of test1 and firstChild is true', succeeded) :-
    dom_element_attribute_value(E, id, test1),
    dom_object_property(_, E, firstChild,C),
    dom_object_method(E, contains(C)).

test('Node', 'isEqualNode of test1 and its deep clone is true', succeeded) :-
    dom_element_attribute_value(E, id, test1),
    dom_object_method(E, cloneNode(true, C)),
    dom_object_method(C, isEqualNode(E)).

test('Node', 'normalize of test1 succeeds', succeeded) :-
    dom_element_attribute_value(E, id, test1),
    dom_object_method(E, normalize).

test('Node', 'removeChild of test1 creates and removes a child node from test1', succeeded) :-
    dom_element_attribute_value(E, id, test1),
    create_dom_element('DIV', N),
    append_dom_node_child(E, N),
    dom_object_method(E, removeChild(N)),
    \+ dom_object_property(_, E, childNode, N).

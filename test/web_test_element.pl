:- ensure_loaded(web_test_utility).

% Properties

test('Element', 'accessKey of test1 finds empty string', succeeded) :-
    dom_element_attribute_value(E, id, test1),
    dom_object_property(_, E, accessKey,'').

test('Element', 'child of test1 finds 1 child with type [htmlelement]', succeeded) :-
    dom_element_attribute_value(E, id, test1),
    findall(T,
                (dom_object_property(_, E, child,C),
                 dom_object_type(C, T)),
                [htmlelement]).

test('Element', 'childElementCount of test1 finds value 1', succeeded) :-
    dom_element_attribute_value(E, id, test1),
    dom_object_property(_, E, childElementCount, 1).

test('Element', 'class of test1 finds 1 classes [foo, bar]', succeeded) :-
    dom_element_attribute_value(E, id, test1),
    setof(C,
                A^dom_object_property(A, E, class,C),
                [bar, foo]).

test('Element', 'clientHeight of test1 finds value 19', succeeded) :-
    dom_element_attribute_value(E, id, test1),
    dom_object_property(_, E, clientHeight, 19).

test('Element', 'generic clientHeight of test1 finds value 19', succeeded) :-
    dom_element_attribute_value(E, id, test1),
    dom_object_property([clientHeight, number], E, clientHeight, 19).

test('Element', 'clientLeft of test1 finds value 1', succeeded) :-
    dom_element_attribute_value(E, id, test1),
    dom_object_property(_, E, clientLeft, 1).

test('Element', 'clientTop of test1 finds value 1', succeeded) :-
    dom_element_attribute_value(E, id, test1),
    dom_object_property(_, E, clientTop, 1).

test('Element', 'clientWidth of test1 finds value > 100', succeeded) :-
    dom_element_attribute_value(E, id, test1),
    dom_object_property(_, E, clientWidth, V),
    V > 100 .

test('Element', 'firstElementChild of test1 finds child with type htmlelement', succeeded) :-
    dom_element_attribute_value(E, id, test1),
    dom_object_property(_, E, firstElementChild,C),
    dom_object_type(C, htmlelement).

test('Element', 'id of test1 is test1', succeeded) :-
    dom_element_attribute_value(E, id, test1),
    dom_object_property(_, E, id, test1).

test('Element', 'innerHTML of test1 is test1Child div"', succeeded) :-
    dom_element_attribute_value(E, id, test1),
    dom_object_property(_, E, innerHTML,
        [10,32,32,32,32,60,100,105,118,32,105,100,61,34,116,101,115,116,49,99,104,105,108,100,34,62,
        10,32,32,32,32,32,32,32,32,116,101,115,116,32,49,32,99,104,105,108,100,
        10,32,32,32,32,60,47,100,105,118,62,10]).

test('Element', 'generic innerHTML of test1 set to "foo"', succeeded) :-
    dom_element_attribute_value(E, id, test1),
    dom_object_property([innerHTML, string], E, innerHTML, OldValue),
    set_dom_object_property(E, innerHTML, "foo", [innerHTML, string, settable]),
    dom_object_property([innerHTML, string], E, innerHTML, "foo"),
    set_dom_object_property(E, innerHTML, OldValue, [innerHTML, string, settable]).

test('Element', 'lastElementChild of test1 finds child with type htmlelement', succeeded) :-
    dom_element_attribute_value(E, id, test1),
    dom_object_property(_, E, lastElementChild,C),
    dom_object_type(C, htmlelement).

test('Element', 'namespaceURI of test1 is "http://www.w3.org/1999/xhtml"', succeeded) :-
    dom_element_attribute_value(E, id, test1),
    dom_object_property(_, E, namespaceURI, "http://www.w3.org/1999/xhtml").

test('Element', 'nextElementSibling of test1 finds a node with type htmlelement', succeeded) :-
    dom_element_attribute_value(E, id, test1),
    dom_object_property(_, E, nextElementSibling,C),
    dom_object_type(C, htmlelement).

test('Element', 'previousElementSibling of nextElementSibling of test1 finds node test1', succeeded) :-
    dom_element_attribute_value(E, id, test1),
    dom_object_property(_, E, nextElementSibling,C),
    dom_object_property(_, C, previousElementSibling,E).

test('Element', 'scrollHeight of test1 finds value 19', succeeded) :-
    dom_element_attribute_value(E, id, test1),
    dom_object_property(_, E, scrollHeight, 19).

test('Element', 'scrollLeft of test1 finds value 0', succeeded) :-
    dom_element_attribute_value(E, id, test1),
    dom_object_property(_, E, scrollLeft, 0).

test('Element', 'scrollTop of test1 finds value 0', succeeded) :-
    dom_element_attribute_value(E, id, test1),
    dom_object_property(_, E, scrollTop, 0).

test('Element', 'scrollWidth of test1 finds value > 100', succeeded) :-
    dom_element_attribute_value(E, id, test1),
    dom_object_property(_, E, scrollWidth, V),
    V > 100 .

test('Element', 'tag of test1 is DIV', succeeded) :-
    dom_element_attribute_value(E, id, test1),
    dom_object_property(_, E, tag, 'DIV').

% Methods

test('Element', 'getBoundingClientRect of test1 is particular rectangle', succeeded) :-
    dom_element_attribute_value(E, id, test1),
    dom_object_method(E, getBoundingClientRect(X)),
    X=dom_rect(8, A, B1, _B2, 8, A, C1, _C2),
    B1 is C1 + 8.
    % X=dom_rect(8,-177,1336,-156,8,-177,1328,21). % Safari
    % X=dom_rect(8,-167,1432,-145,8,-167,1424,21). % Firefox, Mac

test('Element', 'generic getBoundingClientRect of test1 is particular rectangle', succeeded) :-
    dom_element_attribute_value(E, id, test1),
    dom_object_method(E, getBoundingClientRect(X), [getBoundingClientRect, [], dom_rect]),
    X=dom_rect(8, A, B1, _B2, 8, A, C1, _C2),
    B1 is C1 + 8.

test('Element', 'insertAdjacentElement of new div afterEnd of test1', succeeded) :-
    dom_element_attribute_value(E, id, test1),
    create_dom_element('DIV', N),
    dom_object_method(E, insertAdjacentElement(afterEnd, N)),
    dom_object_property(_, N, parentNode, P),
    dom_object_method(P, removeChild(N)).

test('Element', 'insertAdjacentHTML of "a test" afterEnd of test1', succeeded) :-
    dom_element_attribute_value(E, id, test1),
    dom_object_method(E, insertAdjacentHTML(afterEnd, "a test")),
    dom_object_property(_, E, nextSibling, N),
    dom_object_property(_, N, textContent, "a test"),
    dom_object_property(_, N, parentNode, P),
    dom_object_method(P, removeChild(N)).

test('Element', 'generic insertAdjacentHTML of "a test" afterEnd of test1', succeeded) :-
    dom_element_attribute_value(E, id, test1),
    dom_object_method(E, insertAdjacentHTML(afterEnd, "a test"), [insertAdjacentHTML, [position, string_codes]]),
    dom_object_property(_, E, nextSibling, N),
    dom_object_property(_, N, textContent, "a test"),
    dom_object_property(_, N, parentNode, P),
    dom_object_method(P, removeChild(N)).

test('Element', 'insertAdjacentText of "a test" afterEnd of test1', succeeded) :-
    dom_element_attribute_value(E, id, test1),
    dom_object_method(E, insertAdjacentText(afterEnd, "a test")),
    dom_object_property(_, E, nextSibling, N),
    dom_object_property(_, N, textContent, "a test"),
    dom_object_property(_, N, parentNode, P),
    dom_object_method(P, removeChild(N)).

test('Element', 'scrollIntoView of test1 succeeds', succeeded) :-
    dom_element_attribute_value(E, id, test1),
    dom_object_method(E, scrollIntoView([inline-center])).

% parentNode methods.
test('Element', 'querySelector of test1 succeeds', succeeded) :-
    dom_element_attribute_value(E, id, test1),
    dom_object_method(E, querySelector('#test1child', _)).

test('Element', 'querySelectorAll of test1 succeeds', succeeded) :-
    dom_element_attribute_value(E, id, test1),
    setof(T, dom_object_method(E, querySelectorAll('div', T)), _).

test('Element', 'prepend of test1 succeeds', succeeded) :-
    dom_element_attribute_value(E, id, test1),
    dom_object_method(E, prepend(['stuff'])),
    dom_object_property(_, E, childNode, N),
    dom_object_property(_, N, nodeValue, "stuff"),
    dom_object_method(E, removeChild(N)).

test('Element', 'append of test1 succeeds', succeeded) :-
    dom_element_attribute_value(E, id, test1),
    dom_object_method(E, append(['later stuff'])),
    dom_object_property(_, E, childNode, N),
    dom_object_property(_, N, nodeValue, "later stuff"),
    dom_object_method(E, removeChild(N)).

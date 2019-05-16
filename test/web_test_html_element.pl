:- ensure_loaded(web_test_utility).

% Properties

test('HTMLElement', 'contentEditable of test1 finds "inherit"', succeeded) :-
    dom_element_attribute_value(E, id, test1),
    dom_object_property(_, E, contentEditable,inherit).

test('HTMLElement', 'dir of test1 finds ""', succeeded) :-
    dom_element_attribute_value(E, id, test1),
    dom_object_property(_, E, dir,'').

test('HTMLElement', 'innerText of test1 finds "test 1 child[.]"', succeeded) :-
    dom_element_attribute_value(E, id, test1),
    dom_object_property(_, E, innerText, T),
    append("test 1 child", _, T). % Safari follows 'child' with a newline, Firefox/mac has no newline.

test('HTMLElement', 'lang of test1 finds empty string', succeeded) :-
    dom_element_attribute_value(E, id, test1),
    dom_object_property(_, E, lang,'').

test('HTMLElement', 'offsetHeight of test1 finds value 19', succeeded) :-
    dom_element_attribute_value(E, id, test1),
    dom_object_property(_, E, offsetHeight, 21).

test('HTMLElement', 'offsetLeft of test1 finds value 0', succeeded) :-
    dom_element_attribute_value(E, id, test1),
    dom_object_property(_, E, offsetLeft, 8).

test('HTMLElement', 'offsetParent of test1 finds htmlelement object', succeeded) :-
    dom_element_attribute_value(E, id, test1),
    dom_object_property(_, E, offsetParent, P),
    dom_object_type(P, htmlelement).

test('HTMLElement', 'offsetTop of test1 finds value 0', succeeded) :-
    dom_element_attribute_value(E, id, test1),
    dom_object_property(_, E, offsetTop, 8).

test('HTMLElement', 'offsetWidth of test1 finds value > 100', succeeded) :-
    dom_element_attribute_value(E, id, test1),
    dom_object_property(_, E, offsetWidth, V),
    V > 100 .

test('HTMLElement', 'style of test1 finds cssstyledeclaration object', succeeded) :-
    dom_element_attribute_value(E, id, test1),
    dom_object_property(_, E, style, S),
    dom_object_type(S, cssstyledeclaration).

test('HTMLElement', 'tabIndex of test1 finds -1', succeeded) :-
    dom_element_attribute_value(E, id, test1),
    dom_object_property(_, E, tabIndex, -1).

test('HTMLElement', 'title of test1 finds []', succeeded) :-
    dom_element_attribute_value(E, id, test1),
    dom_object_property(_, E, title, []).


% Methods

test('HTMLElement', 'blur of test1 succeeds', succeeded) :-
    dom_element_attribute_value(E, id, test1),
    dom_object_method(E, blur).

test('HTMLElement', 'click of test1 succeeds', succeeded) :-
    dom_element_attribute_value(E, id, test1),
    dom_object_method(E, click).

test('HTMLElement', 'focus of test1 succeeds', succeeded) :-
    dom_element_attribute_value(E, id, test1),
    dom_object_method(E, focus).

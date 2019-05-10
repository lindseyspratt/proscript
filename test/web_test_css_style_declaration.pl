:- ensure_loaded(web_test_utility).
:- ensure_loaded('../library/listut2').

% Properties

test('CSSStyleDeclaration', 'cssText of style of test1 finds "color: blue; border: 1px solid black;"', succeeded) :-
    dom_element_attribute_value(E, id, test1),
    dom_object_property(_, E, style, S),
    dom_object_property(_, S, cssText, Codes),
    % the order of the styles may be changed by previous runs of tests for removeProperty and setProperty.
    contains_list(Codes, "color: blue;"),
    contains_list(Codes, "border: 1px solid black;").

test('CSSStyleDeclaration', 'length of style of test1 finds 18', succeeded) :-
    dom_element_attribute_value(E, id, test1),
    dom_object_property(_, E, style, S),
    dom_object_property(_, S, length, 18).

test('CSSStyleDeclaration', 'parentRule of style of test1 fails', failed) :-
    dom_element_attribute_value(E, id, test1),
    dom_object_property(_, E, style, S),
    dom_object_property(_, S, parentRule, _).

% Methods

test('CSSStyleDeclaration', 'getPropertyPriority(color) of style of test1 fails', succeeded) :-
    dom_element_attribute_value(E, id, test1),
    dom_object_property(_, E, style, S),
    dom_object_method(S, getPropertyPriority(color, '')).

test('CSSStyleDeclaration', 'getPropertyValue(color) of style of test1 finds blue', succeeded) :-
    dom_element_attribute_value(E, id, test1),
    dom_object_property(_, E, style, S),
    dom_object_method(S, getPropertyValue(color, blue)).

test('CSSStyleDeclaration', 'item(0) of style of test1 finds color', succeeded) :-
    dom_element_attribute_value(E, id, test1),
    dom_object_property(_, E, style, S),
    dom_object_method(S, item(0, X)).

test('CSSStyleDeclaration', 'removeProperty(color) of style of test1 finds blue', succeeded) :-
    dom_element_attribute_value(E, id, test1),
    dom_object_property(_, E, style, S),
    dom_object_method(S, removeProperty(color, blue)),
    dom_object_method(S, getPropertyValue(color, '')), % verify the color is removed.
    dom_object_method(S, setProperty(color, blue)). % put the original color back.

test('CSSStyleDeclaration', 'setProperty(color, red) of style of test1 succeeds', succeeded) :-
    dom_element_attribute_value(E, id, test1),
    dom_object_property(_, E, style, S),
    dom_object_method(S, setProperty(color, red)),
    dom_object_method(S, getPropertyValue(color, red)), % verify color is changed
    dom_object_method(S, setProperty(color, blue)). % put the original color back.

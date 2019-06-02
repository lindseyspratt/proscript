:- ensure_loaded(web_test_utility).

% Properties

test('HTMLSelectElement', 'autofocus of select', succeeded) :-
    dom_element_attribute_value(E, id, select),
    dom_object_property(_, E, autofocus,false),
    set_dom_object_property(E, autofocus,true),
    dom_object_property(_, E, autofocus,true),
    set_dom_object_property(E, autofocus,false).

test('HTMLSelectElement', 'disabled of select', succeeded) :-
    dom_element_attribute_value(E, id, select),
    dom_object_property(_, E, disabled,false),
    set_dom_object_property(E, disabled,true),
    dom_object_property(_, E, disabled,true),
    set_dom_object_property(E, disabled,false).

test('HTMLSelectElement', 'form of select', succeeded) :-
    dom_element_attribute_value(E, id, select),
    dom_object_property(_, E, form,X),
    dom_element_attribute_value(X, id, select_form).

test('HTMLSelectElement', 'labels of select', succeeded) :-
    dom_element_attribute_value(E, id, select),
    setof(LabelID, X^T^(dom_object_property(T, E, labels, X), dom_element_attribute_value(X, id, LabelID)), [select_label]).

test('HTMLSelectElement', 'length of select', succeeded) :-
    dom_element_attribute_value(E, id, select),
    dom_object_property(_, E, length,2).

test('HTMLSelectElement', 'multiple of select', succeeded) :-
    dom_element_attribute_value(E, id, select),
    dom_object_property(_, E, multiple,false).

test('HTMLSelectElement', 'name of select', succeeded) :-
    dom_element_attribute_value(E, id, select),
    dom_object_property(_, E, name,'').

test('HTMLSelectElement', 'options of select', succeeded) :-
    dom_element_attribute_value(E, id, select),
    dom_object_property(_, E, options,X).
    setof(OptionValue, X^Y^T^(dom_object_property(T, E, options, X), dom_object_property(Y, X, value, OptionValue)), [a,b]).

test('HTMLSelectElement', 'required of select', succeeded) :-
    dom_element_attribute_value(E, id, select),
    dom_object_property(_, E, required,false).

test('HTMLSelectElement', 'selectedIndex of select', succeeded) :-
    dom_element_attribute_value(E, id, select),
    set_dom_object_property(E, selectedIndex,1),
    dom_object_property(_, E, selectedIndex,1),
    set_dom_object_property(E, selectedIndex,0).

test('HTMLSelectElement', 'selectedOptions of select', succeeded) :-
    dom_element_attribute_value(E, id, select),
    set_dom_object_property(E, selectedIndex,1),
    dom_object_property(T, E, selectedOptions, X),
    dom_object_property(Y, X, value, b),
    set_dom_object_property(E, selectedIndex,0).

    % test using following goal fails when run as part of run_tests. Maybe setof has a problem with multiple uses in the same run of the wam?
    %setof(OptionValue, X^Y^T^(dom_object_property(T, E, selectedOptions, X), dom_object_property(Y, X, value, OptionValue)), [b]),

test('HTMLSelectElement', 'size of select', succeeded) :-
    dom_element_attribute_value(E, id, select),
    dom_object_property(_, E, size,0).

test('HTMLSelectElement', 'type of select', succeeded) :-
    dom_element_attribute_value(E, id, select),
    dom_object_property(_, E, type,'select-one').

test('HTMLSelectElement', 'willValidate of select', succeeded) :-
    dom_element_attribute_value(E, id, select),
    dom_object_property(_, E, willValidate,true).

test('HTMLSelectElement', 'validity of select', succeeded) :-
    dom_element_attribute_value(E, id, select),
    dom_object_property(_, E, validity, State),
    dom_object_type(State, validitystate).

test('HTMLSelectElement', 'validationMessage of select', succeeded) :-
    dom_element_attribute_value(E, id, select),
    dom_object_property(_, E, validationMessage, '').

% Methods

test('HTMLSelectElement', 'add/remove option of select', succeeded) :-
    dom_element_attribute_value(E, id, select),
    dom_create_object('Option'(temp), Option, [string]),
    dom_object_method(E, add(Option)),
    dom_object_property(_, E, length, 3),
    dom_object_method(E, item(2, Option)),
    dom_object_property(_,Option, text, "temp"),
    dom_release_object(Option),
    dom_object_method(E, remove(2)).

test('HTMLSelectElement', 'checkValidity of select', succeeded) :-
    dom_element_attribute_value(E, id, select),
    dom_object_method(E, checkValidity).

% item - tested elsewhere

test('HTMLSelectElement', 'namedItem of select', succeeded) :-
    dom_element_attribute_value(E, id, select),
    dom_object_method(E, namedItem('a-item', X)),
    dom_element_attribute_value(X, name, Y).

% remove - tested elsewhere

test('HTMLSelectElement', 'reportValidity of select', succeeded) :-
    dom_element_attribute_value(E, id, select),
    dom_object_method(E, reportValidity).

test('HTMLSelectElement', 'setCustomValidity of select', succeeded) :-
    dom_element_attribute_value(E, id, select),
    dom_object_method(E, setCustomValidity(custom)),
    \+ dom_object_method(E, checkValidity),
    dom_object_method(E, setCustomValidity('')),
    dom_object_method(E, checkValidity).

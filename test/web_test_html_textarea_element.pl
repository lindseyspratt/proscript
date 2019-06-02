:- ensure_loaded(web_test_utility).

% Properties

test('HTMLTextAreaElement', 'autofocus of textarea', succeeded) :-
    dom_element_attribute_value(E, id, textarea),
    dom_object_property(_, E, autofocus,false),
    set_dom_object_property(E, autofocus,true),
    dom_object_property(_, E, autofocus,true),
    set_dom_object_property(E, autofocus,false).

test('HTMLTextAreaElement', 'cols of textarea', succeeded) :-
    dom_element_attribute_value(E, id, textarea),
    dom_object_property(_, E, cols,20),
    set_dom_object_property(E, cols,50),
    dom_object_property(_, E, cols,50),
    set_dom_object_property(E, cols,20).

test('HTMLTextAreaElement', 'dirName of textarea', succeeded) :-
    dom_element_attribute_value(E, id, textarea),
    dom_object_property(_, E, dirName,''),
    set_dom_object_property(E, dirName,foo),
    dom_object_property(_, E, dirName,foo),
    set_dom_object_property(E, dirName,'').

test('HTMLTextAreaElement', 'disabled of textarea', succeeded) :-
    dom_element_attribute_value(E, id, textarea),
    dom_object_property(_, E, disabled,false),
    set_dom_object_property(E, disabled,true),
    dom_object_property(_, E, disabled,true),
    set_dom_object_property(E, disabled,false).

test('HTMLTextAreaElement', 'form of textarea', succeeded) :-
    dom_element_attribute_value(E, id, textarea),
    dom_object_property(_, E, form,X),
    dom_element_attribute_value(X, id, textarea_form).

test('HTMLTextAreaElement', 'maxLength of textarea', succeeded) :-
    dom_element_attribute_value(E, id, textarea),
    dom_object_property(_, E, maxLength,-1).

test('HTMLTextAreaElement', 'minLength of textarea', succeeded) :-
    dom_element_attribute_value(E, id, textarea),
    dom_object_property(_, E, minLength,-1).

test('HTMLTextAreaElement', 'name of textarea', succeeded) :-
    dom_element_attribute_value(E, id, textarea),
    dom_object_property(_, E, name,'').

test('HTMLTextAreaElement', 'placeholder of textarea', succeeded) :-
    dom_element_attribute_value(E, id, textarea),
    dom_object_property(_, E, placeholder,'').

test('HTMLTextAreaElement', 'readOnly of textarea', succeeded) :-
    dom_element_attribute_value(E, id, textarea),
    dom_object_property(_, E, readOnly,false).

test('HTMLTextAreaElement', 'required of textarea', succeeded) :-
    dom_element_attribute_value(E, id, textarea),
    dom_object_property(_, E, required,false).

test('HTMLTextAreaElement', 'rows of textarea', succeeded) :-
    dom_element_attribute_value(E, id, textarea),
    dom_object_property(_, E, rows,2),
    set_dom_object_property(E, rows,10),
    dom_object_property(_, E, rows,10),
    set_dom_object_property(E, rows,2).

test('HTMLTextAreaElement', 'wrap of textarea', succeeded) :-
    dom_element_attribute_value(E, id, textarea),
    dom_object_property(_, E, wrap,'').

test('HTMLTextAreaElement', 'type of textarea', succeeded) :-
    dom_element_attribute_value(E, id, textarea),
    dom_object_property(_, E, type,textarea).

test('HTMLTextAreaElement', 'defaultValue of textarea', succeeded) :-
    dom_element_attribute_value(E, id, textarea),
    dom_object_property(_, E, defaultValue,'    ').

test('HTMLTextAreaElement', 'value of textarea', succeeded) :-
    dom_element_attribute_value(E, id, textarea),
    set_dom_object_property(E, value,test),
    dom_object_property(_, E, value,test).

test('HTMLTextAreaElement', 'textLength of textarea', succeeded) :-
    dom_element_attribute_value(E, id, textarea),
    set_dom_object_property(E, value,test),
    dom_object_property(_, E, textLength, 4).

test('HTMLTextAreaElement', 'willValidate of textarea', succeeded) :-
    dom_element_attribute_value(E, id, textarea),
    dom_object_property(_, E, willValidate, true).

test('HTMLTextAreaElement', 'validity of textarea', succeeded) :-
    dom_element_attribute_value(E, id, textarea),
    dom_object_property(_, E, validity, State),
    dom_object_type(State, validitystate).

test('HTMLTextAreaElement', 'validationMessage of textarea', succeeded) :-
    dom_element_attribute_value(E, id, textarea),
    dom_object_property(_, E, validationMessage, '').

test('HTMLTextAreaElement', 'labels of textarea', succeeded) :-
    dom_element_attribute_value(E, id, textarea),
    setof(LabelID, X^T^(dom_object_property(T, E, labels, X), dom_element_attribute_value(X, id, LabelID)), [label1, label2]).

test('HTMLTextAreaElement', 'selectionStart of textarea', succeeded) :-
    dom_element_attribute_value(E, id, textarea),
    dom_object_property(_, E, selectionStart, 0).

test('HTMLTextAreaElement', 'selectionEnd of textarea', succeeded) :-
    dom_element_attribute_value(E, id, textarea),
    dom_object_method(E, setRangeText(foo, 0, 50, select)),
    dom_object_property(_, E, selectionEnd, 3).

test('HTMLTextAreaElement', 'selectionDirection of textarea', succeeded) :-
    dom_element_attribute_value(E, id, textarea),
    dom_object_property(_, E, selectionDirection, none).

% Methods

test('HTMLTextAreaElement', 'checkValidity of textarea', succeeded) :-
    dom_element_attribute_value(E, id, textarea),
    dom_object_method(E, checkValidity).

test('HTMLTextAreaElement', 'reportValidity of textarea', succeeded) :-
    dom_element_attribute_value(E, id, textarea),
    dom_object_method(E, reportValidity).

test('HTMLTextAreaElement', 'setCustomValidity of textarea', succeeded) :-
    dom_element_attribute_value(E, id, textarea),
    dom_object_method(E, setCustomValidity(custom)),
    \+ dom_object_method(E, checkValidity),
    dom_object_method(E, setCustomValidity('')),
    dom_object_method(E, checkValidity).

test('HTMLTextAreaElement', 'select of textarea', succeeded) :-
    dom_element_attribute_value(E, id, textarea),
    dom_object_method(E, select).

test('HTMLTextAreaElement', 'setRangeText of textarea', succeeded) :-
    dom_element_attribute_value(E, id, textarea),
    dom_object_method(E, setRangeText(foo, 0, 50, select)),
    dom_object_property(_, E, value, foo).

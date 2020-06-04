:- ensure_loaded(web_test_utility).
:- ensure_loaded('../library/listut2').

% Properties

test('HTMLInputElement', 'accept of input ', succeeded) :-
    dom_element_attribute_value(E, id, input_checkbox),
    dom_object_property(_, E, accept,'').

test('HTMLInputElement', 'alt of input ', succeeded) :-
    dom_element_attribute_value(E, id, input_image),
    dom_object_property(_, E, alt,"alternate").

test('HTMLInputElement', 'autocomplete of input ', succeeded) :-
    dom_element_attribute_value(E, id, input_text),
    set_dom_object_property(E, autocomplete,on),
    dom_object_property(_, E, autocomplete,on).

test('HTMLInputElement', 'autofocus of input ', succeeded) :-
    dom_element_attribute_value(E, id, input_text),
    dom_object_property(_, E, autofocus,false).

test('HTMLInputElement', 'defaultChecked of input ', succeeded) :-
    dom_element_attribute_value(E, id, input_checkbox),
    dom_object_property(_, E, defaultChecked, false).

test('HTMLInputElement', 'checked of input ', succeeded) :-
    dom_element_attribute_value(E, id, input_checkbox),
    dom_object_property(_, E, checked, false).

test('HTMLInputElement', 'dirName of input ', succeeded) :-
    dom_element_attribute_value(E, id, input_checkbox),
    dom_object_property(_, E, dirName, '').

test('HTMLInputElement', 'disabled of input ', succeeded) :-
    dom_element_attribute_value(E, id, input_checkbox),
    dom_object_property(_, E, disabled, false).

test('HTMLInputElement', 'form of input ', succeeded) :-
    dom_element_attribute_value(E, id, input_checkbox),
    dom_object_property(_, E, form,X),
    dom_element_attribute_value(X, id, input_form1).

% files: Returns/accepts a FileList object, which contains a list of File objects representing the files selected for upload.

test('HTMLInputElement', 'files of input ', succeeded) :-
    dom_element_attribute_value(E, id, input_file),
    \+ dom_object_property(_, E, files,_). % no file is chosen.

test('HTMLInputElement', 'formAction of input ', succeeded) :-
    dom_element_attribute_value(E, id, input_checkbox),
    dom_object_property(_, E, formAction,X),
    atom_codes(X, Xs),
    contains_list(Xs, "tests.html").

test('HTMLInputElement', 'formEnctype of input ', succeeded) :-
    dom_element_attribute_value(E, id, input_checkbox),
    dom_object_property(_, E, formEnctype,'').

test('HTMLInputElement', 'formMethod of input ', succeeded) :-
    dom_element_attribute_value(E, id, input_checkbox),
    dom_object_property(_, E, formMethod,'').

test('HTMLInputElement', 'formNoValidate of input ', succeeded) :-
    dom_element_attribute_value(E, id, input_checkbox),
    dom_object_property(_, E, formNoValidate,false).

test('HTMLInputElement', 'formTarget of input ', succeeded) :-
    dom_element_attribute_value(E, id, input_checkbox),
    dom_object_property(_, E, formTarget,'').

test('HTMLInputElement', 'height of input ', succeeded) :-
    dom_element_attribute_value(E, id, input_image),
    dom_object_property(_, E, height,17).

test('HTMLInputElement', 'indeterminate of input ', succeeded) :-
    dom_element_attribute_value(E, id, input_checkbox),
    dom_object_property(_, E, indeterminate,false).

test('HTMLInputElement', 'max of input ', succeeded) :-
    dom_element_attribute_value(E, id, input_number),
    dom_object_property(_, E, max,'93').

test('HTMLInputElement', 'maxLength of input ', succeeded) :-
    dom_element_attribute_value(E, id, input_text),
    dom_object_property(_, E, maxLength,32).

test('HTMLInputElement', 'min of input ', succeeded) :-
    dom_element_attribute_value(E, id, input_number),
    dom_object_property(_, E, min,'').

test('HTMLInputElement', 'minLength of input ', succeeded) :-
    dom_element_attribute_value(E, id, input_text),
    dom_object_property(_, E, minLength,-1).

test('HTMLInputElement', 'multiple of input ', succeeded) :-
    dom_element_attribute_value(E, id, input_text),
    dom_object_property(_, E, multiple,false).

test('HTMLInputElement', 'name of input ', succeeded) :-
    dom_element_attribute_value(E, id, input_file),
    dom_object_property(_, E, name,file_input).

test('HTMLInputElement', 'pattern of input ', succeeded) :-
    dom_element_attribute_value(E, id, input_text),
    dom_object_property(_, E, pattern,'').

test('HTMLInputElement', 'placeholder of input ', succeeded) :-
    dom_element_attribute_value(E, id, input_text),
    dom_object_property(_, E, placeholder,'').

test('HTMLInputElement', 'readOnly of input ', succeeded) :-
    dom_element_attribute_value(E, id, input_text),
    dom_object_property(_, E, readOnly,false).

test('HTMLInputElement', 'required of input ', succeeded) :-
    dom_element_attribute_value(E, id, input_text),
    dom_object_property(_, E, required,false).

test('HTMLInputElement', 'size of input ', succeeded) :-
    dom_element_attribute_value(E, id, input_text),
    dom_object_property(_, E, size,20).

test('HTMLInputElement', 'src of input ', succeeded) :-
    dom_element_attribute_value(E, id, input_image),
    dom_object_property(_, E, src,'').

test('HTMLInputElement', 'step of input ', succeeded) :-
    dom_element_attribute_value(E, id, input_number),
    dom_object_property(_, E, step,'7').

test('HTMLInputElement', 'type of input ', succeeded) :-
    dom_element_attribute_value(E, id, input_number),
    dom_object_property(_, E, type, number).

test('HTMLInputElement', 'defaultValue of input ', succeeded) :-
    dom_element_attribute_value(E, id, input_number),
    dom_object_property(_, E, defaultValue, '').

test('HTMLInputElement', 'value of input ', succeeded) :-
    dom_element_attribute_value(E, id, input_number),
    dom_object_property(_, E, value, '').

test('HTMLInputElement', 'valueAsNumber of input ', succeeded) :-
    dom_element_attribute_value(E, id, input_text),
    dom_object_property(_, E, valueAsNumber, _X).

test('HTMLInputElement', 'width of input ', succeeded) :-
    dom_element_attribute_value(E, id, input_image),
    dom_object_property(_, E, width,50).

test('HTMLInputElement', 'willValidate of input ', succeeded) :-
    dom_element_attribute_value(E, id, input_text),
    dom_object_property(_, E, willValidate,true).

test('HTMLInputElement', 'validity of input', succeeded) :-
    dom_element_attribute_value(E, id, input_text),
    dom_object_property(_, E, validity, State),
    dom_object_type(State, validitystate).

test('HTMLInputElement', 'validationMessage of input', succeeded) :-
    dom_element_attribute_value(E, id, input_text),
    dom_object_property(_, E, validationMessage, '').

test('HTMLInputElement', 'labels of input', succeeded) :-
    dom_element_attribute_value(E, id, input_text),
    setof(LabelID, X^T^(dom_object_property(T, E, labels, X), dom_element_attribute_value(X, id, LabelID)), [input_text_label]).

test('HTMLInputElement', 'selectionStart of input', succeeded) :-
    dom_element_attribute_value(E, id, input_text),
    dom_object_property(_, E, selectionStart, 0).

test('HTMLInputElement', 'selectionEnd of input', succeeded) :-
    dom_element_attribute_value(E, id, input_text),
    dom_object_method(E, setRangeText(foo, 0, 50, select)),
    dom_object_property(_, E, selectionEnd, 3).

test('HTMLInputElement', 'selectionDirection of input', succeeded) :-
    dom_element_attribute_value(E, id, input_text),
    dom_object_property(_, E, selectionDirection, none).

% Methods

test('HTMLInputElement', 'stepUp of input', succeeded) :-
    dom_element_attribute_value(E, id, input_number),
    set_dom_object_property(E, value, '7'),
    dom_object_method(E, stepUp(2)),
    dom_object_property(_, E, value, '21'),
    set_dom_object_property(E, value, '').

test('HTMLInputElement', 'stepDown of input', succeeded) :-
    dom_element_attribute_value(E, id, input_number),
    set_dom_object_property(E, value, '7'),
    dom_object_method(E, stepDown(3)),
    dom_object_property(_, E, value, '-14'),
    set_dom_object_property(E, value, '').

test('HTMLInputElement', 'checkValidity of input', succeeded) :-
    dom_element_attribute_value(E, id, input_text),
    dom_object_method(E, checkValidity).

test('HTMLInputElement', 'reportValidity of input', succeeded) :-
    dom_element_attribute_value(E, id, input_text),
    dom_object_method(E, reportValidity).

test('HTMLInputElement', 'setCustomValidity of input', succeeded) :-
    dom_element_attribute_value(E, id, input_text),
    dom_object_method(E, setCustomValidity(custom)),
    \+ dom_object_method(E, checkValidity),
    dom_object_method(E, setCustomValidity('')),
    dom_object_method(E, checkValidity).

test('HTMLInputElement', 'select of input', succeeded) :-
    dom_element_attribute_value(E, id, input_text),
    dom_object_method(E, select).

test('HTMLInputElement', 'setRangeText of input', succeeded) :-
    dom_element_attribute_value(E, id, input_text),
    dom_object_method(E, setRangeText(foo, 0, 50, select)),
    dom_object_property(_, E, value, foo).



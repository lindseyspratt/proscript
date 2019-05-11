:- ensure_loaded(web_test_utility).

% Properties

test('HTMLCanvasElement', 'height of canvas finds 100', succeeded) :-
    dom_element_attribute_value(E, id, canvas),
    dom_object_property(_, E, height,100).

test('HTMLCanvasElement', 'width of canvas finds 300', succeeded) :-
    dom_element_attribute_value(E, id, canvas),
    dom_object_property(_, E, width,300).

% Methods

test('HTMLCanvasElement', 'getContext(2d, C) of canvas returns object with type canvasrenderingcontext2d', succeeded) :-
    dom_element_attribute_value(E, id, canvas),
    dom_object_method(E, getContext('2d', C)),
    dom_object_type(C, canvasrenderingcontext2d).

test('HTMLCanvasElement', 'toBlob(U) of canvas returns data URL codes with length > 100', succeeded) :-
    dom_element_attribute_value(E, id, canvas),
    dom_object_method(E, toBlob([object-X] ^ dom_object_type(X, _))).

/*
% not yet supported: needs create_dom_object_url
copy_blob(X) :-
    create_dom_element('IMG', Img),
    create_dom_object_url(X, U),
    dom_object_method(Img, addEventListener(load, revoke_dom_object_url(U))),
    set_dom_object_property(_, Img, src, U),
    dom_object_property(element, Body, tag, body),
    append_dom_node_child(Body, Img).

canvas.toBlob(function(blob) {
  var newImg = document.createElement('img'),
      url = URL.createObjectURL(blob);

  newImg.onload = function() {
    // no longer need to read the blob so it's revoked
    URL.revokeObjectURL(url);
  };

  newImg.src = url;
  document.body.appendChild(newImg);
});
*/

test('HTMLCanvasElement', 'toDataURL(U) of canvas returns data URL codes with length > 100', succeeded) :-
    dom_element_attribute_value(E, id, canvas),
    dom_object_method(E, toDataURL(U)),
    length(U, L), L > 100.


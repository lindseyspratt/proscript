:- ensure_loaded(web_test_utility).
:- dynamic(test_result/1).

test('Blob', 'create strings', succeeded) :-
    retractall(test_result(_)),
    dom_create_object('Blob'([a,b]), Blob, [array_type(string)]),
    dom_object_type(Blob, blob).

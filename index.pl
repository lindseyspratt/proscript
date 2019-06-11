:- ensure_loaded('library/object').
:- initialization(init).

init :-
    setof(Toggler, Toggler >-> class :> caret, Togglers),
    init(Togglers).

init([]).
init([H|T]) :-
    H >*> addEventListener(click, toggle(H)),
    init(T).

toggle(Toggler) :-
    Toggler >+> parentElement :> P,
    P >+> child :> C,
    C >-> class :> nested,
    toggle_dom_element_class(C, active, _),
    toggle_dom_element_class(Toggler, 'caret-down', _).

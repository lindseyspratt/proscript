:- ensure_loaded('object').
:- dynamic(item/2).
:- initialization(init).

init :-
    setof(Toggler, Toggler >-> class :> caret, Togglers),
    init(Togglers),
    register_items,
    _ >> [class -:> body, addEventListener(scroll, update_index_visibility_emphasis)].

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

register_items :-
    forall(
        (Content >> [class -:> indexed, id -:> ID],
         atom_concat(ID, '_index', IndexID),
         Index >-> id :> IndexID),
        assertz(item(Content, Index))).

update_index_visibility_emphasis :-
    _ >> [class -:> body, scrollTop +:> ScrollTop, clientHeight +:> ClientHeight],
    BodyVisibleBottom is ScrollTop + ClientHeight,
    forall(item(Content, Index),
        toggle_visibility(ScrollTop, BodyVisibleBottom, Content, Index)).

toggle_visibility(ScrollTop, BodyVisibleBottom, Content, Index) :-
    overlaps(ScrollTop, BodyVisibleBottom, Content)
      -> (Index >-> class :> visible
            -> true
          ;
          toggle_dom_element_class(Index, visible, add)
         )
    ;
    Index >-> class :> visible
      -> toggle_dom_element_class(Index, visible, remove)
    ;
    true.

overlaps(ScrollTop, BodyVisibleBottom, Content) :-
    Content >+> offsetTop :> ContentTop,
    (ScrollTop =< ContentTop,
     BodyVisibleBottom >= ContentTop
      -> true
    ;
     Content >+> offsetHeight :> ContentHeight,
     ContentBottom is ContentTop + ContentHeight,
     ScrollTop > ContentTop,
     ScrollTop =< ContentBottom
      -> true
    ).

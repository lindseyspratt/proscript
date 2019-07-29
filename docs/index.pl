:- ensure_loaded('../library/object').
:- dynamic(item/2).
:- initialization(init).

init :-
    set_initial_active,
    activate_parents,
    setof(IndexLink, IndexLink >-> class :> index, Links),
    handle_links(Links),
    setof(Toggler, Toggler >-> class :> caret, Togglers),
    init(Togglers),
    register_items,
    _ >> [class -:> body, addEventListener(scroll, update_index_visibility_emphasis)].

set_initial_active :-
    url_entry_point_id(ID)
      -> atom_concat(ID, '_index', MenuID),
         X >-> id :> MenuID,
         active_link(X)
    ;
    true. % leave main menu node 'active'.

url_entry_point_id(ID) :-
    dom_window(W),
    W >+> document :> D,
    D >+> 'URL' :> URLCodes,
    [HashCode] = "#",
    append(_, [HashCode|IDCodes], URLCodes),
    atom_codes(ID, IDCodes).

activate_parents :-
    C >-> class :> active,
    activate_parents(C).


activate_parents(C) :-
    C >+> parentElement :> P
      -> activate_element(P),
         activate_parents(P)
    ;
     true.

activate_element(P) :-
    P >-> class :> nested
      -> toggle_dom_element_class(P, active, add),
         (P >+> parentElement :> PP
           -> PP >+> child :> C,
              C >-> class :> caret,
              toggle_dom_element_class(C, 'caret-down', add)
          ;
          true
         )
    ;
    true.

handle_links([]).
handle_links([H|T]) :-
    H >*> addEventListener(click, active_link(H)),
    handle_links(T).

active_link(Link) :-
     C >-> [class :> active, class :> index], % C is the current active node of the sidenav menu.
     toggle_dom_element_class(C, active, remove),
     toggle_dom_element_class(Link, active, add).

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

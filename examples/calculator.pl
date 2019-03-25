
init :-
  asserta(current("0")),
  asserta(memory("0")),
  asserta(operation(0)),
  asserta(maxlength(30)).


contains_list(List, Sublist) :-
    contains_list(List, _, Sublist, _).

contains_list(List, Prefix, Sublist, Suffix) :-
    append(Sublist, Suffix, Tail),
    append(Prefix, Tail, List).

currentx(X) :-
    clause(current(X), true).

current_in_error :-
    currentx(Current),
    contains_list(Current, "!").

current_has_period :-
    currentx(Current),
    contains_list(Current, ".").

current_has_e0 :-
    currentx(Current),
    contains_list(Current, "e0").

current_too_long :-
    currentx(Current),
    length(Current, Length),
    maxlength(Maxlength),
    Length > Maxlength.

extract_e_from_current :-
    retract(current(Current)),
    contains_list(Current, Prefix, "e", Suffix),
    append(Prefix, Suffix, X),
    asserta(current(X)).

set_current(X) :-
    retractall(current(_)),
    asserta(current(X)).

add_current(X) :-
    retract(current(C)),
    append(C, X, Y),
    asserta(current(Y)).

lowercase_current :-
    retract(current(C)),
    lowercase(C, Y),
    asserta(current(Y)).

lowercase([H|T], [LH|LT]) :-
    lowercase1(H, LH),
    lowercase(T, LT).
lowercase([], []).

case_map("ABCDEFGHIJKLMNOPQRSTUVWXYZ", "abcdefghijklmnopqrstuvwxyz").

lowercase1(X, LX) :-
    case_map(Us, Ls),
    lowercase1(Us, X, Ls, LX).

lowercase1([], X, _, X).
lowercase1([U|Us], X, [L|Ls], Y) :-
    U = X
      -> L = Y
    ;
    lowercase1(Us, X, Ls, Y).

set_dom_name_path_value([H|T], V) :-
    append("[name=", H, Select),
    dom_select_element(Select, E),
    set_dom_name_path_value(T, E, V).

set_dom_name_path_value([], E, V) :-
    set_dom_element_attribute_value(E, value, V).
set_dom_name_path_value([H|T], E, V) :-
    dom_element_child(E, C),
    dom_element_attribute_value(C, name, H),
    set_dom_name_path_value(T, C, V).

add_digit(Dig) :-
    (\+ current_in_error
      -> (eval_current(0),
          \+ current_has_period
           -> set_current(Dig)
         ;
          add_current(Dig)
         ),
         lowercase_current
    ;
    set_current("Hint! Press 'AC'")
    ),
    (current_has_e0
      -> extract_e_from_current
    ; true
    ),
    (current_too_long
      -> set_current("Aargh! Too long")
    ; true
    ).

dot :-
    (current_length(0)
      -> set_current("0.")
    ;
    \+ current_has_period,
    \+ current_has_e
      -> add_current(".")
    ),
    currentx(Current),
    set_dom_name_path_value(["Calculator", "Display"], Current).


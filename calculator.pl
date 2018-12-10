current("0").
memory("0").
operation(0).
maxlength(30).


contains_list(List, Sublist) :-
    contains_list(List, _, Sublist, _).

contains_list(List, Prefix, Sublist, Suffix) :-
    append(Sublist, Suffix, Tail),
    append(Prefix, Tail, List).

current_in_error :-
    current(Current),
    contains_list(Current, "!").

current_has_period :-
    current(Current),
    contains_list(Current, ".").

current_has_e0 :-
    current(Current),
    contains_list(Current, "e0").

current_too_long :-
    current(Current),
    length(Current, Length),
    maxlength(Maxlength)
    Length > Maxlength.

extract_e_from_current :-
    retract(current(Current)),
    contains_list(Current, Prefix, "e", Suffix),
    append(Prefix, Suffix, X),
    assert(current(X)).

set_current(X) :-
    retractall(current(_)),
    assert(current(X)).

add_current(X) :-
    retract(current(C)),
    append(C, X, Y),
    assert(current(Y)).

lowercase_current :-
    retract(current(C)),
    lowercase(C, Y),
    assert(current(Y)).

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
    dom_select_element(Select, E)
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

function Dot()                  //PUT IN "." if appropriate.
{
    if ( Current.length == 0)     //no leading ".", use "0."
    { Current = "0.";
    } else
    {  if (   ( Current.indexOf(".") == -1)
        &&( Current.indexOf("e") == -1)
    )
    { Current = Current + ".";
    };   };
    document.Calculator.Display.value = Current;
}

dot :-
    (current_length(0)
      -> set_current("0.")
    ;
    \+ current_has_period,
    \+ current_has_e
      -> add_current(".")
    ),
    current(Current),
    set_dom_name_path_value(["Calculator", "Display"], Current).


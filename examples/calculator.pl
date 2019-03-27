
init :-
  asserta(current("0")),
  asserta(memory("0")),
  asserta(operation(0)),
  asserta(maxlength(30)).

fix_current.

contains_list(List, Sublist) :-
    contains_list(List, _, Sublist, _).

contains_list(List, Prefix, Sublist, Suffix) :-
    append(Sublist, Suffix, Tail),
    append(Prefix, Tail, List).

currentx(X) :-
    clause(current(X), true),
    !.
currentx("0").

current_length(L) :-
    currentx(X),
    length(X, L).

current_is_blank :-
    currentx(" ").

current_in_error :-
    currentx(Current),
    contains_list(Current, "!").

current_has_period :-
    currentx(Current),
    contains_list(Current, ".").

current_has_e0 :-
    currentx(Current),
    contains_list(Current, "e0").

current_has_e :-
    currentx(Current),
    contains_list(Current, "e").

current_too_long :-
    current_length(Length),
    maxlength(Maxlength),
    Length > Maxlength.

set_current(X) :-
    retractall(current(_)),
    asserta(current(X)).

add_current(X) :-
    retract(current(C)),
    append(C, X, Y),
    asserta(current(Y)),
    !.

memoryx(X) :-
    clause(memory(X), true).

set_memory(X) :-
    retractall(memory(_)),
    asserta(memory(X)).

operationx(X) :-
    clause(operation(X), true).

set_operation(X) :-
    retractall(operation(_)),
    asserta(operation(X)).

lowercase_current :-
    retract(current(C)),
    lowercase(C, Y),
    asserta(current(Y)),
    !.

lowercase([H|T], [LH|LT]) :-
    lowercase1(H, LH),
    !,
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

display_current :-
    currentx(Current),
    set_dom_name_path_value(["Calculator", "Display"], Current).

/*
The 'name' path is not required to be sequential. A path of [a,b] names some node
with name 'a' that has a sub-node with name 'b'. A node p1 has children q21 and q22 and q21
has children r31 and r32 and q22 has children s31 and s32. If q22 has name 'a' and s31 has name 'b', then path
of [a,b] would select s31.
Since multiple nodes may have the same name this predicate may set values on multiple leaf nodes.
*/
set_dom_name_path_value([H|T], V) :-
    append("[name=", H, SelectPrefix),
    append(SelectPrefix, "]", Select),
    dom_select_element(Select, E),
    (V = [_|_] -> atom_codes(VA, V);VA = V),
    set_dom_name_path_value(T, E, VA).

set_dom_name_path_value([], E, V) :-
    set_dom_element_attribute_value(E, value, V).
% Set value for leaf in path with a direct child with name H and sub-nodes for path T.
set_dom_name_path_value([H|T], E, V) :-
    dom_element_property(E, child, C),
    atom_codes(HA, H),
    dom_element_attribute_value(C, name, HA),
    set_dom_name_path_value(T, C, V).
% Set value for leaf in path with an indirect child with name H and sub-nodes for path T.
set_dom_name_path_value([H|T], E, V) :-
    dom_element_property(E, child, C),
    set_dom_name_path_value([H|T], C, V).

add_digit(Dig) :-
    number_codes(Dig, DigCodes),
    (\+ current_in_error
      -> (current_is_blank
           -> set_current(DigCodes)
         ;
          eval_current(0),
          \+ current_has_period
           -> set_current(DigCodes)
         ;
          add_current(DigCodes)
         ),
         lowercase_current
    ;
    set_current("Hint! Press 'AC'")
    ),
    (current_has_e0
      -> update_current_collapse_e0
    ;
    current_has_eneg0
      -> update_current_collapse_eneg0
    ; true
    ),
    (current_too_long
      -> set_current("Aargh! Too long")
    ; true
    ),
    display_current.

add_exponent :-
    currentx(Current),
    append(Current, "e0", NewCurrent),
    set_current(NewCurrent),
    display_current.

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

eval_current(R) :-
    currentx(C),
    eval_codes(C, R).

% eval codes with 'e': e.g. "2.1e-3"
eval_codes(C, R) :-
    append(PrefixE, Suffix, C),
    append(Prefix, "e", PrefixE),
    !,
    eval_codes(Prefix, P),
    eval_codes(Suffix, S),
    R is P * 10 ^ S.

eval_codes(C, R) :-
    catch(
      (atom_codes(A, C),
       atom_to_term(A, T, _),
       call(R is T)),
      _,
      fail).

clear :-
    set_current("0"),
    display_current.

all_clear :-
    set_memory("0"),
    set_current("0"),
    set_operation(0),
    display_current.

current_has_eneg0 :-
    currentx(Current),
    contains_list(Current, "e-0").

current_has_eneg :-
    currentx(Current),
    contains_list(Current, "e-").

current_has_neg :-
    currentx(Current),
    append("-", _, Current).

replace_in_list(List, Old, New, NewList) :-
     contains_list(List, Prefix, Old, Suffix),
     append(Prefix, New, PrefixWithNew),
     append(PrefixWithNew, Suffix, NewList).

update_current_collapse_e0 :-
    currentx(Current),
    replace_in_list(Current, "e0", "e", NewCurrent),
    set_current(NewCurrent).

update_current_collapse_eneg0 :-
    currentx(Current),
    replace_in_list(Current, "e-0", "e-", NewCurrent),
    set_current(NewCurrent).

update_current_remove_neg_exponent :-
    currentx(Current),
    replace_in_list(Current, "e-", "e", NewCurrent),
    set_current(NewCurrent).

update_current_insert_neg_exponent :-
    currentx(Current),
    replace_in_list(Current, "e", "e-", NewCurrent),
    set_current(NewCurrent).

update_current_remove_neg :-
    currentx(Current),
    append("-", NewCurrent, Current),
    set_current(NewCurrent).

update_current_insert_neg :-
    currentx(Current),
    append("-", Current, NewCurrent),
    set_current(NewCurrent).

plus_minus :-
    (current_has_e
      -> (current_has_eneg
            -> update_current_remove_neg_exponent
          ;
          update_current_insert_neg_exponent
         )
    ;
    current_has_neg
      -> update_current_remove_neg
    ;
    update_current_insert_neg
    ),
    (eval_current(0),
    \+ current_has_period
      -> set_current("0")
    ; true
    ),
    display_current.

do_exponent :-
    current_has_e
      -> true
    ;
    add_exponent.

operate(Op) :-
    operationx(CurrentOp),
    (CurrentOp \= 0 -> calculate;true),
    atom_codes(Op, [OpCode]),
    nth(OpCode, "*/+-", NewOp),
    !,
    set_operation(NewOp),
    currentx(Current),
    set_memory(Current),
    set_current(" "),
    display_current.

nth(X, L, N) :-
    nth(X, L, 1, N).

nth(X, [X|_], N, N).
nth(X, [_|T], J, N) :-
    K is J + 1,
    nth(X, T, K, N).

calculate :-
  operationx(OpNum),
  memoryx(Memory),
  currentx(Current),
  eval_codes(Memory, MV),
  eval_codes(Current, CV),
  eval_operation(OpNum, MV, CV, Value),
  set_operation(0),
  set_memory("0"),
  (number(Value)
    -> number_codes(Value, ValueCodes)
  ;
  atom_codes(Value, ValueCodes)
  ),
  set_current(ValueCodes),
  display_current.

eval_operation(1, MV, CV, Value) :- Value is MV * CV.
eval_operation(2, MV, CV, Value) :- Value is MV / CV.
eval_operation(3, MV, CV, Value) :- Value is MV + CV.
eval_operation(4, MV, CV, Value) :- Value is MV - CV.

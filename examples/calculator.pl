:- dynamic([current/1, memory/1, operation/1, maxlength/1]).

init :-
  asserta(current("0")),
  asserta(memory("0")),
  asserta(operation(0)),
  asserta(maxlength(30)).

fix_current.

currentx(X) :-
    clause(current(X), true),
    !.
currentx("0").

current_length(L) :-
    currentx(X),
    length(X, L).

maxlengthx(X) :-
    clause(maxlength(X), true).

current_prop_string(has_e, "e").
current_prop_string(has_e0, "e0").
current_prop_string(has_period, ".").
current_prop_string(in_error, "!").
current_prop_string(is_blank, " ").
current_prop_string(has_eneg0, "e-0").
current_prop_string(has_eneg, "e-").

current_prop(has_neg) :-
    currentx(Current),
    append("-", _, Current).

current_prop(too_long) :-
    !,
    current_length(Length),
    maxlengthx(Maxlength),
    Length > Maxlength.

current_prop(Prop) :-
    current_prop_string(Prop, String),
    currentx(Current),
    contains_list(Current, String).


set_current(X) :-
    retractall(current(_)),
    asserta(current(X)).

add_current(X) :-
    retract(current(C)),
    append(C, X, Y),
    asserta(current(Y)),
    !.


update_old_new(collapse_e0, "e0", "e").
update_old_new(collapse_eneg0, "e-0", "e-").
update_old_new(remove_neg_exponent, "e-", "e").
update_old_new(insert_neg_exponent, "e", "e-").


update_current(remove_neg) :-
    !,
    currentx(Current),
    append("-", NewCurrent, Current),
    set_current(NewCurrent).

update_current(insert_neg) :-
    !,
    currentx(Current),
    append("-", Current, NewCurrent),
    set_current(NewCurrent).

update_current(append_e0) :-
    !,
    currentx(Current),
    append(Current, "e0", NewCurrent),
    set_current(NewCurrent).

update_current(lowercase) :-
    !,
    currentx(Current),
    lowercase(Current, NewCurrent),
    set_current(NewCurrent).

update_current(Key) :-
    update_old_new(Key, Old, New),
    update_current(Old, New).


update_current(Old, New) :-
    currentx(Current),
    replace_in_list(Current, Old, New, NewCurrent),
    set_current(NewCurrent).


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

display_current :-
    currentx(Current),
    set_dom_name_path_value(["Calculator", "Display"], Current).

add_digit(Dig) :-
    number_codes(Dig, DigCodes),
    (\+ current_prop(in_error)
      -> (current_prop(is_blank)
           -> set_current(DigCodes)
         ;
          eval_current(0),
          \+ current_prop(has_period)
           -> set_current(DigCodes)
         ;
          add_current(DigCodes)
         ),
         update_current(lowercase)
    ;
    set_current("Hint! Press 'AC'")
    ),
    (current_prop(has_e0)
      -> update_current(collapse_e0)
    ;
    current_prop(has_eneg0)
      -> update_current(collapse_eneg0)
    ; true
    ),
    (current_prop(too_long)
      -> set_current("Aargh! Too long")
    ; true
    ),
    display_current.

add_exponent :-
    update_current(append_e0),
    display_current.

dot :-
    (current_length(0)
      -> set_current("0.")
    ;
    \+ current_prop(has_period),
    \+ current_prop(has_e)
      -> add_current(".")
    ),
    display_current.

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

plus_minus :-
    (current_prop(has_e)
      -> (current_prop(has_eneg)
            -> update_current(remove_neg_exponent)
          ;
          update_current(insert_neg_exponent)
         )
    ;
    current_prop(has_neg)
      -> update_current(remove_neg)
    ;
    update_current(insert_neg)
    ),
    (eval_current(0),
    \+ current_prop(has_period)
      -> set_current("0")
    ; true
    ),
    display_current.

do_exponent :-
    current_prop(has_e)
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

% === Utilities ===
%
% These utility predicates are generally useful. They include an HTML DOM predicate set_dom_name_path_value/2
% and four list utilities.

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

replace_in_list(List, Old, New, NewList) :-
     contains_list(List, Prefix, Old, Suffix),
     append(Prefix, New, PrefixWithNew),
     append(PrefixWithNew, Suffix, NewList).

contains_list(List, Sublist) :-
    contains_list(List, _, Sublist, _).

contains_list(List, Prefix, Sublist, Suffix) :-
    append(Sublist, Suffix, Tail),
    append(Prefix, Tail, List).

nth(X, L, N) :-
    nth(X, L, 1, N).

nth(X, [X|_], N, N).
nth(X, [_|T], J, N) :-
    K is J + 1,
    nth(X, T, K, N).

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

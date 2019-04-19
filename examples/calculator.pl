% Purpose: Support a simple HTML Calculator. The calculator allows positive and negative
% numbers including exponent notation. It supports four operators: +, -, *, and /.
%
% Author: Lindsey Spratt, 2019.
%
% Implementation: This calculator implementation uses clauses to store
% state (implemented in calculator_data.pl).
% 'current' is the current register value. This is
% where digits input from the user are held and
% where results of operations are displayed.
% 'memory' holds the previous value of 'current'.
% Clicking on an operator in the interface records
% the index (1 - 4) of that operator, copies 'current'
% to 'memory', and resets 'current' to empty.
% Clicking on '=' in the interface runs the
% calculate/0 predicate that evaluates
% 'operator'('memory', 'current') to get new
% vaue for 'current'.


:- ensure_loaded(calculator_data).
:- ensure_loaded('../library/dom.pl').
:- ensure_loaded('../library/listut.pl').
:- ensure_loaded('../library/listut2.pl').

fix_current.

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
    select_list(Old, Current, New, NewCurrent),
    set_current(NewCurrent).


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
    split_list(C, Prefix, "e", Suffix),
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
    nth1(NewOp, "*/+-", OpCode),
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

:- module(system_util, [write_list/2, write_list/3, capitalize/2]).

write_list(List, Separator) :-
    current_output(Stream),
    write_list(List, Separator, Stream).

write_list([], _, _).
write_list([H|T], Separator, Stream) :-
        write(Stream, H),
        write_list1(T, Separator, Stream).

write_list1([], _, _).
write_list1([H|T], Separator, Stream) :-
        write(Stream, Separator),
        write(Stream, H),
        write_list1(T, Separator, Stream).

capitalize(A, CA) :-
    atom_codes(A, Xs),
    Xs = [X|Codes],
    capitalize_code(X, CX),
    atom_codes(CA, [CX|Codes]).

capitalize_code(X, CX) :-
    "a" = [AC],
    "z" = [ZC],
    (AC =< X, X =< ZC ->
      "A" = [AAC],
      K is X - AC,
      CX is K + AAC
     ;
     CX = X
    ).

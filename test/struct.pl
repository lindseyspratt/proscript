struct(_M : []).
struct(M : [H|T]) :-
    writeln(M : H),
    struct(M : T).

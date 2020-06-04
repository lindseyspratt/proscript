listut2_dummy_reference :-
    listut2_dummy_reference,
    select_list(_, _, _, _).

append_lists([], []).
append_lists([L], L) :- !.
append_lists([H1, H2|T], R) :-
    append(H1, N, R),
    append_lists([H2|T], N).


%   select_list(X, Xlist, Y, Ylist)
%
%   is true when X is a sublist of Xlist and Y is a sublist of Ylist,
%   and apart from that Xlist and Ylist are the same: the portion
%   of Xlist that precedes X, Prefix, is the same as the portion of
%   Ylist that precedes Y, and the portion of Xlist that follows
%   X, Suffix, is the same as the portion of Ylist that follows
%   Y. Roughly, Xlist = Prefix + X + Suffix and Ylist = Prefix + Y + Suffix.
%   append_lists([Prefix, X, Suffix], Xlist), append_lists([Prefix, Y, Suffix], Ylist).
%   You can use it to replace X by Y or vice versa.

select_list(Old, List, New, NewList) :-
     split_list(List, Prefix, Old, Suffix),
     append(Prefix, New, PrefixWithNew),
     append(PrefixWithNew, Suffix, NewList).

contains_list([], []).
contains_list([H|T], [H|U]) :-
    contains_list1(T, U).
contains_list([_|T], S) :-
    contains_list(T, S).

contains_list1(_, []).
contains_list1([H|T], [H|U]) :-
    contains_list1(T, U).

split_list(List, Prefix, Sublist, Suffix) :-
    append(Sublist, Suffix, Tail),
    append(Prefix, Tail, List).


case_map("ABCDEFGHIJKLMNOPQRSTUVWXYZ", "abcdefghijklmnopqrstuvwxyz").

lowercase([H|T], [LH|LT]) :-
    lowercase1(H, LH),
    !,
    lowercase(T, LT).
lowercase([], []).

lowercase1(X, LX) :-
    case_map(Us, Ls),
    lowercase1(Us, X, Ls, LX).

lowercase1([], X, _, X).
lowercase1([U|Us], X, [L|Ls], Y) :-
    U = X
      -> L = Y
    ;
    lowercase1(Us, X, Ls, Y).


uppercase([H|T], [LH|LT]) :-
    uppercase1(H, LH),
    !,
    uppercase(T, LT).
uppercase([], []).

uppercase1(X, UX) :-
    case_map(Us, Ls),
    uppercase1(Ls, X, Us, UX).

uppercase1([], X, _, X).
uppercase1([L|Ls], X, [U|Us], Y) :-
    L = X
      -> U = Y
    ;
    uppercase1(Us, X, Ls, Y).

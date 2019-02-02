debug(Goal) :-
    write('Call '), writenl(Goal),
    call(Goal),
    write('Exit '), writenl(Goal).



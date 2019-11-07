factorial(0, 1):- !.
factorial(1, 1).
factorial(N, Factorial):-
   NN is N-1,
   factorial(NN, F),
   Factorial is N * F.

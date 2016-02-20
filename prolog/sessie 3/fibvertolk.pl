fib(0,1).
fib(1,1).
fib(N,F) :-
N > 1,
N2 is N - 2,
fib(N2,F2),
N1 is N - 1,
fib(N1,F1),
F is F1 + F2.

vertolk((G1,G2)) :- !, vertolk(G1), vertolk(G2).
vertolk(true) :- !.
vertolk(A > B) :-!, A > B.
vertolk(A is B):- !, A is B.
vertolk(Head) :- clause(Head,Body), vertolk(Body).


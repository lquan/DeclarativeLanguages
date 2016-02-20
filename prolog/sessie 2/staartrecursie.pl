a(0,S) :- !, statistics(localused,S).
a(X,S) :-
 Y is X - 1,
 a(Y,S).
 
b(0,S) :- !, statistics(localused,S).
b(X,S) :-
 Y is X - 1,
 b(Y,S),
 X > 0.

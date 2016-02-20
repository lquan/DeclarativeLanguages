inc(X,Y) :- Y is X + 1.

map([],_,[]).
map([X|T1],P,[Y|T2]):-
	T=..[P,X,Y],
	call( T ),
	map(T1,P,T2).

vertolk((G1,G2)) :- !, vertolk(G1), vertolk(G2).
vertolk(A=..B):-!, A =.. B.
vertolk(call(C)):- !,vertolk(C).
vertolk(A is B):- !, A is B.
vertolk(true) :- !.
vertolk(Head) :- clause(Head,Body), vertolk(Body).
%vertolk((G1,G2,G3)) :- !, vertolk



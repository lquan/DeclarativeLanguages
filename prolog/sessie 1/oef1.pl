vader(anton,bart).
vader(anton,daan).
vader(anton,elisa).
vader(fabian,anton).
moeder(celine,bart).
moeder(celine,daan).
moeder(celine,gerda).
moeder(gerda,hendrik).

sibling(X,Y) :- vader(Z,X), 
		vader(Z,Y), 
		X \== Y,
		moeder(M,X), 
		moeder(M,Y).
voorvader(X,Y) :- vader(X,Y).
voorvader(X,Y) :-
		voorvader(X,Z),
		vader(Z,Y).


doceert(demoen,bs).
doceert(demoen,cc).
doceert(olivie,bvp).
doceert(olivie,mm).
doceert(verbaeten,socs).
doceert(dedecker,socs).

volgt(tom,bs).
volgt(tom,bvp).
volgt(tom,socs).
volgt(maarten,socs).
volgt(maarten,bs).
volgt(pieter,bvp).

volgt_zelfde_vak(X,Y) :-% volgt(X,B), volgt(Y,B), X\==Y, X @< Y.
	findall(
	(X,Y),
	(volgt(Y,Z),volgt(X,Z),X\==Y),
	L), 
	sort(L,S),
	member((X,Y),S) .

doceert_zelfde_vak(X,Y) :- 
	findall((X,Y),(doceert(Y,Z),doceert(X,Z),X\==Y),
	L), 
	sort(L,S),
	member((X,Y),S) .

doceert_meer_een_vak(X) :-
	findall(X, (doceert(X,Y),doceert(X,Z), Y\==Z),L),
	sort(L,L1), 
	%length(L1,Length), 
	%Length > 1, 
	member(X,L1).


        



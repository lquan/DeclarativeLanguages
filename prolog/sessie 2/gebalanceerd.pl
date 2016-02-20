diepte(leeg, 0).
diepte(knoop(L,_,R),D) :- diepte(L,D1), diepte(R,D2), Acc is max(D1,D2), D is Acc+1.

gebalanceerd(leeg).
gebalanceerd(knoop(L,_,R)) :-
	diepte(L, D1), diepte(R, D2), abs(D1-D2) =< 1,
	gebalanceerd(L), gebalanceerd(R).


voeg_in(leeg,E,knoop(leeg,E,leeg)).
voeg_in(knoop(leeg,W,R), E, knoop(knoop(leeg,E,leeg),W,R)).
voeg_in(knoop(L,W,leeg), E, knoop(L,W,knoop(leeg,E,leeg))).

%voeg_in(B,E,Result) :- diepte(L,D1), diepte(R,D2), D1 =< D2, voeg_in(L,E,Result),
%B = knoop(L,_,R), Result = 
				 
voeg_in(knoop(L,W,R),E,knoop(Result,W,R)) :- diepte(L,D1), diepte(R,D2), D1 =< D2, voeg_in(L,E,Result).
voeg_in(knoop(L,W,R),E,knoop(L,W,Result)) :- diepte(L,D1), diepte(R,D2), D1 > D2, voeg_in(E,R,Result).


%diepte(tree(LT, _, RT), H) :-
%height(LT,H1), height(RT,H2), Hm is max(H1,H2), H is HM+1.

%gebalanceerd(leeg, 0).
%gebalanceerd(knoop(L,_,R), H) :-
%gebalanceerd(L, H1), gebalanceerd(R, H1), H is H1 + 1.
%gebalanceerd(leeg,_,R):- R.
%gebalanceerd(_,
%gebalanceerd(L,W,R):-

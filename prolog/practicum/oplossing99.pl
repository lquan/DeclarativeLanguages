:-ensure_loaded(opgave).
%bord( [ (1,1,eiland(1)), (2,1,_) , (3,1,eiland(1)), ]).
bord([  (1,1,eiland(2)),  (1,2,_), (1,3,eiland(2)),
	(2,1,_), 	  (2,2,_), (2,3,_), 
	(3,1,eiland(2)),  (3,2,_), (3,3,eiland(2)) 
	]).

hashi(Bord) :-
	findall((R,K,eiland(Q)), (member((R,K,eiland(Q)),Bord),nonvar(Q)), X),
	X = [Eiland1|_],
	losop_eiland(Bord, Eiland1, X).

test(B) :-
	bord(B),
	%zoek_eilanden(B,X),
	findall((R,K,eiland(Q)), (member((R,K,eiland(Q)),B),nonvar(Q)), X),
	X = [Eiland1|_],
	losop_eiland(B, Eiland1, X).


eiland_opgelost(Bord, (R,K,eiland(Quota))) :-
	aantal_bruggen((R,K),Bord,Quota).

losop_eiland(Bord, (Rij,Kolom,eiland(Q1)), Onopgelost) :-
	losop_eiland(Bord, (Rij,Kolom,eiland(Q1)), Onopgelost, [(Rij,Kolom,eiland(Q1))]).
	
%los het eiland op Rij, Kolom op
losop_eiland(Bord, _, [], Visited) :-
	findall((R,K,eiland(Q)), (member((R,K,eiland(Q)),Bord),nonvar(Q)), Eilanden),
	sort(Visited,Vsorted), length(Vsorted,X),
	length(Eilanden,X).
		
%losop_eiland(Bord, (Rij,Kolom,eiland(Q1)), Onopgelost, Visited) :-
	%het eiland is al opgelost (bruggen == quota)
	%eiland_opgelost(Bord, (Rij,Kolom,eiland(Q1))),
	%verwijder hem uit de onopgeloste lijst
	%delete(Onopgelost, (Rij,Kolom,eiland(Q1)), Onopgelost1),
	
	%length(Onopgelost,L),
	%check lengte als er maar twee overgebleven zijn
	%( 
	%(L =:= 2 -> losop_eiland(Bord, Onopgelost, Onopgelost, Visited)
	%;
	%(
	%[OnHead,OnTail] = Onopgelost1, 
	%%los nu de andere ongeloste op
	%losop_eiland(Bord, OnHead, OnTail, Visited)
	%)
	%)
	%.


losop_eiland(Bord, (Rij,Kolom,eiland(Q1)), Onopgelost, Visited) :-
	Visited = [VisitedLast|_],
	( (eiland_opgelost(Bord, (Rij,Kolom,eiland(Q1)))) 
	->
		(
		%verwijder hem uit de onopgeloste lijst
		delete(Onopgelost, (Rij,Kolom,eiland(Q1)), Onopgelost1),
		losop_eiland(Bord, VisitedLast, Onopgelost1, Visited)
		)
		
		;
		
		(
		member(AnderEiland,Onopgelost), 
		AnderEiland \== (Rij,Kolom,eiland(Q1)),
		AnderEiland \== VisitedLast,
		
		(%connecteer_hor(Bord, (Rij,Kolom,eiland(Q1)), 2, AnderEiland),
		connecteer_hor(Bord, (Rij,Kolom,eiland(Q1)), 1, AnderEiland)
		; 
		%connecteer_ver(Bord, (Rij,Kolom,eiland(Q1)), 2, AnderEiland),
		connecteer_ver(Bord, (Rij,Kolom,eiland(Q1)), 1, AnderEiland)
		),
		
		( (eiland_opgelost(Bord, AnderEiland))
		  ->
		  	(
		  	delete(Onopgelost, AnderEiland , Rest1),
		  	losop_eiland(Bord, (Rij,Kolom,eiland(Q1)), Rest1, [AnderEiland|Visited])
		  	)
		  	;
		  	
		  	(
		  	losop_eiland(Bord, (Rij,Kolom,eiland(Q1)), Onopgelost, [AnderEiland|Visited])
		  	)
		
		)
	)
	).

%losop_eiland(Bord, (Rij,Kolom,eiland(Q1)), Onopgelost, Visited) :-
%	%\+ eiland_opgelost(Bord, (Rij,Kolom,eiland(Q1))),
	% een onopgelost eiland moet nog bruggen 
	% naar een ander onopgelost en niet het juist vorige eiland hebben
%	Visited = [VisitedLast|_],
%	member(AnderEiland,Onopgelost), AnderEiland \== (Rij,Kolom,eiland(Q1)),
%	%( (length(Onopgelost,X), X > 1) -> AnderEiland \== VisitedLast),
%	AnderEiland \== VisitedLast,
%	
	%(%connecteer_hor(Bord, (Rij,Kolom,eiland(Q1)), 2, AnderEiland),
%	connecteer_hor(Bord, (Rij,Kolom,eiland(Q1)), 1, AnderEiland)
%	; 
%	%connecteer_ver(Bord, (Rij,Kolom,eiland(Q1)), 2, AnderEiland),
%	connecteer_ver(Bord, (Rij,Kolom,eiland(Q1)), 1, AnderEiland)
%	),
%	
%	(eiland_opgelost(Bord, AnderEiland) ->
%		%kijken of dat eiland opgelost is door deze brug
%		( delete(Onopgelost, AnderEiland , Rest1),
%		%los dit eiland verder op met de resterende
%		( eiland_opgelost(Bord,(Rij,Kolom,eiland(Q1))) ->
%			(delete(Rest1, (Rij,Kolom,eiland(Q1)) , [Rest2|Tail]),
%			losop_eiland(Bord, Rest2, Tail, [AnderEiland|Visited]),
%			)
%		%losop_eiland(Bord, (Rij,Kolom,eiland(Q1)), Rest1, [AnderEiland|Visited])
%		)
%		;
		% anders gewoon doordoen met dit eiland en de originele%
		%append(Visited,[AnderEiland],Temp),%
%		losop_eiland(Bord, (Rij,Kolom,eiland(Q1)), Onopgelost, [AnderEiland|Visited])	
%	).	

% maak een horizontale brug tussen deze 2 eilanden met Breedte, zelfde rij
connecteer_hor(Bord, (R,K1,eiland(Q1)), Breedte, (R,K2,eiland(Q2))) :-
	 %eiland 2 staat rechts (minstens 1 leeg vakje van eerste eiland) 
	K2-K1 > 1,
	Breedte =< Q1,
	Breedte =< Q2,
	K is K1 + 1,
	connecteer_hor(Bord, (R,K1,eiland(Q1)), K, Breedte, (R,K2,eiland(Q2))).
%%%de accumulator houdt bij waar we zijn (rechts van het eerste eiland)

%%maak brug en ga naar rechts...
connecteer_hor(Bord, (R,K1,eiland(Q1)), Acc, Breedte, (R,K2,eiland(Q2))) :-
	Acc < K2,
	memberchk( (R,Acc,brug(h,Breedte)),Bord),
	Acc1 is Acc + 1,	
	connecteer_hor(Bord, (R,K1,eiland(Q1)), Acc1,Breedte, (R,K2,eiland(Q2))).
%%%...totdat we hebben het eiland bereikt hebben. (Acc = K2)
connecteer_hor(_, (R,_,_), K2, _, (R,K2,_)).

%analoog voor verticale bruggen
connecteer_ver(Bord, (R1,K,eiland(Q1)), Breedte, (R2,K,eiland(Q2))) :-
	R2-R1 > 1,
	Breedte =< Q1,
	Breedte =< Q2,
	R is R1 + 1,
	connecteer_ver(Bord, (R1,K,eiland(Q1)), R, Breedte, (R2,K,eiland(Q2))).

connecteer_ver(Bord, (R1,K,eiland(Q1)), Acc,Breedte, (R2,K,eiland(Q2))) :-
	Acc < R2,
	memberchk( (Acc,K,brug(v,Breedte)),Bord),
	Acc1 is Acc + 1,	
	connecteer_ver(Bord, (R1,K,eiland(Q1)), Acc1,Breedte, (R2,K,eiland(Q2))).

connecteer_ver(_, (_,K,_), R2,_, (R2,K,_)).

%%%%%%%%%%%%%%%%%%
%buurpredicaten
linkerbuur((R,K),(R,K1,I),B) :-
	K > 1,
	K1 is K - 1,
	memberchk((R,K1,I),B).
rechterbuur((R,K),(R,K1,I),B) :-
	K1 is K + 1, %dimensiecheck??
	memberchk((R,K1,I),B).
bovenbuur((R,K),(R1,K,I),B) :-
	R > 1,
	R1 is R - 1,
	memberchk((R1,K,I),B).
onderbuur((R,K),(R1,K,I),B) :-
	R1 is R + 1, %dimensiecheck??
	memberchk((R1,K,I),B).

buur((R,K),(R1,K1,I),Bord) :-
	linkerbuur((R,K),(R1,K1,I),Bord);
	rechterbuur((R,K),(R1,K1,I),Bord);
	bovenbuur((R,K),(R1,K1,I),Bord);
	onderbuur((R,K),(R1,K1,I),Bord).

% hoeveel bruggen zijn verbonden aan het vakje (R,K)?
aantal_bruggen((R,K),Bord,Aantal) :-
	%zoek de buren die bruggen zijn
	findall(A, (buur((R,K),(_,_,I1),Bord), nonvar(I1), I1 = brug(_,A)), L),
	sumlist(L,Aantal).


%%%%%%%%%%%%%%%%temp
%zoek de eilanden
%zoek_eilanden(B,L) :-
%	findall((R,K,eiland(Q)), (member((R,K,eiland(Q)),B),nonvar(Q)), L).

%aantal rijen en kolommen van het bord
%dimensies_bord(Bord,R,K) :-
%	last(Bord,(R,K,_)).


	

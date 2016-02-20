:-ensure_loaded(opgave).
:-ensure_loaded(gui).
brug_breedte(2).
brug_breedte(1).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
hashi(Bord) :-
	zoek_eilanden(Bord, Eilanden),
	eiland_brug_buren(Eilanden,Map),
	losop_eiland(Bord, Eilanden, Map).

test(B,Key,Value) :-
	bord(B),%bord([(1,1,_),(1,2,eiland(4)),(1,3,_),(1,4,_),(1,5,_),(1,6,eiland(2)),
	zoek_eilanden(B,Eilanden),
	eiland_brug_buren(Eilanden,Map),
	%losop_eiland(B, Eilanden, Map).
	gen_assoc(Key,Map,Value).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
verbind(Bord, (R1,K1,eiland(Q1)), Breedte, (R2,K2,eiland(Q2))) :-
	%eiland_quota(Bord,(R1,K1,eiland(Q1)),Vrij1), Vrij1 >= 1, 
	%Breedte =< Vrij1,
	eiland_quota(Bord,(R2,K2,eiland(Q2)),Vrij2), Vrij2 >= 1, 
	Breedte =< Vrij2,
	(verbind_hor(Bord, (R1,K1,eiland(Q1)), Breedte, (R2,K2,eiland(Q2)))
	;
	verbind_ver(Bord, (R1,K1,eiland(Q1)), Breedte, (R2,K2,eiland(Q2)))).
%%%%%%%%%%%%%%%%%%%%%%%%%%%horizontale brug
verbind_hor(Bord, (R,K1,eiland(Q1)), Breedte, (R,K2,eiland(Q2))) :-
	K2-K1 > 1,
	K is K1+1,
	verbind_hor(Bord, (R,K,eiland(Q1)), Breedte, K, (R,K2,eiland(Q2))).

verbind_hor(Bord, (R,K1,eiland(Q1)), Breedte, (R,K2,eiland(Q2))) :-
	K1-K2 > 1,
	verbind_hor(Bord, (R,K2,eiland(Q1)), Breedte, (R,K1,eiland(Q2))).

verbind_hor(_, (R,_,_), _, K, (R,K,_)).
verbind_hor(Bord, (R,K1,eiland(Q1)), Breedte, Acc, (R,K2,eiland(Q2))) :-
	Acc < K2,
	memberchk((R,Acc,brug(h,Breedte)),Bord),
	Acc1 is Acc+1,
	verbind_hor(Bord, (R,K1,eiland(Q1)), Breedte, Acc1, (R,K2,eiland(Q2))).

verbind_ver(Bord, (R1,K,eiland(Q1)), Breedte, (R2,K,eiland(Q2))) :-
	R2-R1 > 1,
	R is R1+1,
	verbind_ver(Bord, (R1,K,eiland(Q1)), Breedte, R, (R2,K,eiland(Q2))).

verbind_ver(Bord, (R1,K,eiland(Q1)), Breedte, (R2,K,eiland(Q2))) :-
	R1-R2 > 1,
	verbind_ver(Bord, (R2,K,eiland(Q1)), Breedte, (R1,K,eiland(Q2))).

verbind_ver(_, (_,K,_), _, R, (R,K,_)).
verbind_ver(Bord, (R1,K,eiland(Q1)), Breedte, Acc, (R2,K,eiland(Q2))) :-
	Acc < R2,
	memberchk((Acc,K,brug(v,Breedte)),Bord),
	Acc1 is Acc+1,
	verbind_ver(Bord, (R1,K,eiland(Q1)), Breedte, Acc1, (R1,K,eiland(Q2))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%zoek de eilanden
zoek_eilanden(B,L) :-
	findall((R,K,eiland(Q)), (member((R,K,eiland(Q)),B), nonvar(Q)), L).

%hoeveel bruggen kunnen we nog verbinden?
eiland_quota(Bord, (R,K,eiland(Quota)), Vrij) :-
	aantal_bruggen(Bord,(R,K,eiland(Quota)),Bruggen),
	Vrij is Quota - Bruggen.

eiland_opgelost(Bord, (R,K,eiland(Q))) :- eiland_quota(Bord, (R,K,eiland(Q)), 0).

% hoeveel bruggen zijn verbonden?
aantal_bruggen(Bord, (R,K,_), Aantal) :-
	%zoek de buren die bruggen zijn volgens de juiste richting
	findall(A, ((K1 is K+1; K1 is K-1),memberchk((R,K1,brug(h,A)),Bord), nonvar(A)), L1),
	findall(B, ((R1 is R+1; R1 is R-1),memberchk((R1,K,brug(v,B)),Bord), nonvar(B)), L2),
	sumlist(L1,A1),
	sumlist(L2,A2),
	Aantal is A1 + A2.

eiland_brug_buren(Eilanden, Map) :-
	findall(Eiland-BurenLijst, (member(Eiland,Eilanden),eiland_brug_buren(Eiland,Eilanden,BurenLijst)), Lijst),
	list_to_assoc(Lijst,Map).

%een eiland heeft volgende brugburen
eiland_brug_buren((R,K,eiland(Q)), Eilanden, Lijst) :-
	findall((R,K1,eiland(Q1)), member((R,K1,eiland(Q1)),Eilanden), Horizontaal),
	findall((R1,K,eiland(Q2)), member((R1,K,eiland(Q2)),Eilanden), Verticaal),
	
	findall((R,K3,eiland(Q3)), 
		( (nextto((R,K3,eiland(Q3)),(R,K,eiland(Q)),Horizontaal);
		   nextto((R,K,eiland(Q)),(R,K3,eiland(Q3)),Horizontaal)),
		abs(K3-K) > 1),
		Hor),

	findall((R4, K, eiland(Q4)),
		( (nextto((R4, K, eiland(Q4)), (R,K,eiland(Q)), Verticaal);
		  nextto((R,K,eiland(Q)), (R4,K,eiland(Q4)), Verticaal)), 
		abs(R4-R) > 1),
		Ver),
	append(Hor,Ver,Lijst).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%initialisatie	
losop_eiland(Bord, [Eiland1|Rest], Map) :-
	losop_eiland(Bord, Eiland1, Map, [Eiland1|Rest]).
%geen onopgeloste meer
losop_eiland(_, _,_, []).
%los het gegeven eiland op gegeven map en de lijst van de andere onopgeloste eilanden
losop_eiland(Bord, Eiland1, Map, Onopgelost) :-
	eiland_quota(Bord, Eiland1, RestQuota),
	get_assoc(Eiland1, Map, BurenLijst),
	member(BuurEiland,BurenLijst), member(BuurEiland,Onopgelost),
	member(Breedte1,[2,1]),
	verbind(Bord,Eiland1,Breedte1,BuurEiland),	
	eiland_quota(Bord, Eiland1, RestQuota),
	select(Eiland1, Onopgelost, Onopgelost1),

	(RestQuota > 0 ->
		(member(BuurEiland2, BurenLijst), member(BuurEiland2, Onopgelost), 
		BuurEiland2 \== BuurEiland,
		member(Breedte2,[2,1]),
		verbind(Bord,Eiland1,Breedte2,BuurEiland2),
		eiland_quota(Bord, Eiland1, RestQuota2),

		(RestQuota2 > 0 -> 
			(
			(member(BuurEiland3, BurenLijst), member(BuurEiland3, Onopgelost), 
			BuurEiland3 \== BuurEiland1, BuurEiland3 \== BuurEiland2,
			member(Breedte3,[2,1]),
			verbind(Bord,Eiland1,Breedte3,BuurEiland3),
			eiland_quota(Bord, Eiland1, RestQuota3)
			(RestQuota3 > 0 ->
				(member(BuurEiland4, BurenLijst), member(BuurEiland4, Onopgelost), 
				BuurEiland4 \== BuurEiland1, BuurEiland4 \== BuurEiland3, BuurEiland4 \= BuurEiland2,
				member(Breedte4,[2,1]),
				verbind(Bord,Eiland1,Breedte4,BuurEiland3),
				eiland_quota(Bord, Eiland1, 0),
				losop_eiland(Bord, BuurEiland2, Map, Onopgelost1),
				losop_eiland(Bord, BuurEiland3, Map, Onopgelost1),
				losop_eiland(Bord, BuurEiland4, Map, Onopgelost1)
				)
				;
				(
				losop_eiland(Bord, BuurEiland2, Map, Onopgelost1),
				losop_eiland(Bord, BuurEiland3, Map, Onopgelost1)
				)				
			)
			;
			losop_eiland(Bord, BuurEiland2, Map, Onopgelost1))))
		);
		losop(Bord,Eiland1,Map,Onopgelost1)).
	

		
		



	%update_gui(Bord),
	get_assoc(Eiland1, Map, BurenLijst),
	member(AnderEiland,BurenLijst), member(AnderEiland,Onopgelost),
	findall(B, brug_breedte(B), Breedtes),
	member(Breedte,Breedtes),
	verbind(Bord,Eiland1,Breedte,AnderEiland),
	eiland_quota(Bord, Eiland1, RestQuota),
	(RestQuota =:= 0 ->
		(delete(Onopgelost, Eiland1, [Onopgelost1|Rest]),
		losop_eiland(Bord, Onopgelost1, Map, Rest))
		;
		(nextto(AnderEiland, AnderEiland2, BurenLijst), member(AnderEiland2, Onopgelost),
		member(Breedte2,Breedtes),
		verbind(Bord,Eiland1,Breedte2,AnderEiland2),
		losop_eiland(Bord, Onopgelost1, Map, Rest))
	).

	%eiland_quota(Bord, (Eiland1), RestQuota),
	%(RestQuota =:= 0 ->
	%	(delete(Onopgelost, Eiland1, [Onopgelost1|Rest]),
	%	losop_eiland(Bord, Onopgelost1, Map, Rest))
	%	;
	%	(member(AnderEiland,BurenLijst), member(AnderEiland,Onopgelost),	
	%	brug_breedte(Breedte),
	%	verbind(Bord,Eiland1,Breedte,AnderEiland),
	%	eiland_quota(Bord, Eiland1, RestQuota1),
	%	(RestQuota1 > 0 ->
	%		(nextto(AnderEiland, AnderEiland2, BurenLijst), member(AnderEiland2, Onopgelost),
	%		brug_breedte(Breedte2),
	%		verbind(Bord,Eiland1,Breedte2,AnderEiland2),
	%		losop_eiland(Bord, Eiland1, Map, Onopgelost))
	%		;
	%		losop_eiland(Bord, Eiland1, Map, Onopgelost))
	%	)		
	%).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
bord([  (1,1,eiland(2)),  (1,2,_), (1,3,eiland(2)),
	(2,1,_), 	  (2,2,_), (2,3,_), 
	(3,1,eiland(2)),  (3,2,_), (3,3,eiland(2)) 
	]).	
bord([(1,1,_),(1,2,eiland(4)),(1,3,_),(1,4,_),(1,5,_),(1,6,eiland(2)),
(2,1,_),(2,2,_),(2,3,_),(2,4,_),(2,5,_),(2,6,_),
(3,1,_),(3,2,_),(3,3,_),(3,4,eiland(1)),(3,5,_),(3,6,_),
(4,1,_),(4,2,_),(4,3,_),(4,4,_),(4,5,_),(4,6,_),
(5,1,_),(5,2,eiland(1)),(5,3,_),(5,4,eiland(3)),(5,5,_),(5,6,eiland(2))]).

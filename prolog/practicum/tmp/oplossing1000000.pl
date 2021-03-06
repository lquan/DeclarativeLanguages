:-ensure_loaded(opgave).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
hashi(Bord) :-
	zoek_eilanden(Bord, Eilanden),
	eiland_brug_buren(Eilanden,Map),
	losop_eiland(Bord, Eilanden, Map,_).
test(B,Mapout,Opgelost) :-
	bord(B),
	zoek_eilanden(B, Eilanden),
	eiland_brug_buren(Eilanden,Map),
	losop_eiland(B,Eilanden,Map,Mapout),
	relaxeer(B,Eilanden,Map,Opgelost).
%
relaxeer(Bord, Eilanden, Map, MoetenDubbele) :-
	findall((R,K,eiland(N)), 
		(member((R,K,eiland(N),Eilanden)), 
			N mod 2 =:= 0, 
			get_assoc((R,K,eiland(N)), Map, (BurenLijst,_)), 
			length(BurenLijst), 
			Burenlijst =:=N/2),
		 MoetenDubbele).
%	verbind_dubbel(Bord,MoetenDubbele,Map,[]).
	
%verbind_dubbel(Bord,MoetenDubbele,Map,[]) :-
%	verbind_dubbel(Bord,MoetenDubbele,Map,Acc).
%verbind_dubbel(Bord,MoetenDubbele,Map,Acc) :-
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
verbind(Bord,Map, (R,K1,eiland(Q1)),(R,K2,eiland(Q2)),MapOut) :-
	eiland_quota(Bord,(R,K1,eiland(Q1)),Vrij1), 
	eiland_quota(Bord,(R,K2,eiland(Q2)),Vrij2),
	member(Breedte,[2,1]),
	Breedte =< Vrij1, Breedte =< Vrij2, 
	verbind_hor(Bord, (R,K1,eiland(Q1)), Breedte, (R,K2,eiland(Q2))),
	get_assoc((R,K1,eiland(Q1)), Map, (BurenLijst1,VerbondenLijst1), Map1, (BurenLijst1,[(R,K2,eiland(Q2))|VerbondenLijst1])),
	get_assoc((R,K2,eiland(Q2)), Map1, (BurenLijst2,VerbondenLijst2), MapOut, (BurenLijst2,[(R,K2,eiland(Q1))|VerbondenLijst2])).
	
verbind(Bord,Map, (R1,K,eiland(Q1)),(R2,K,eiland(Q2)), MapOut) :-
	eiland_quota(Bord,(R1,K,eiland(Q1)),Vrij1), 
	eiland_quota(Bord,(R2,K,eiland(Q2)),Vrij2),
	member(Breedte,[2,1]),
	Breedte =< Vrij1, Breedte =< Vrij2, 
	verbind_ver(Bord, (R1,K,eiland(Q1)), Breedte, (R2,K,eiland(Q2))),
	get_assoc((R1,K,eiland(Q1)), Map, (BurenLijst1,VerbondenLijst1), Map1, (BurenLijst1,[(R2,K,eiland(Q2))|VerbondenLijst1])),
	get_assoc((R2,K,eiland(Q2)), Map1, (BurenLijst2,VerbondenLijst2), MapOut, (BurenLijst2,[(R1,K,eiland(Q1))|VerbondenLijst2])).

verbind_hor(Bord, (R,K1,eiland(Q1)), Breedte, (R,K2,eiland(Q2))) :-
	K2-K1 > 1,
	K is K1+1,
	verbind_hor(Bord, (R,K,eiland(Q1)), Breedte, K, (R,K2,eiland(Q2))).

verbind_hor(Bord, (R,K1,eiland(Q1)), Breedte, (R,K2,eiland(Q2))) :-
	K1-K2 > 1,
	verbind_hor(Bord, (R,K2,eiland(Q2)), Breedte, (R,K1,eiland(Q1))).

verbind_hor(_, (R,_,eiland(_)), _, K, (R,K,eiland(_))).
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
	verbind_ver(Bord, (R2,K,eiland(Q2)), Breedte, (R1,K,eiland(Q1))).

verbind_ver(_, (_,K,eiland(_)), _, R, (R,K,eiland(_))).
verbind_ver(Bord, (R1,K,eiland(Q1)), Breedte, Acc, (R2,K,eiland(Q2))) :-
	Acc < R2,
	memberchk((Acc,K,brug(v,Breedte)),Bord),
	Acc1 is Acc+1,
	verbind_ver(Bord, (R1,K,eiland(Q1)), Breedte, Acc1, (R2,K,eiland(Q2))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%zoek de eilanden
zoek_eilanden(B,L) :-
	findall((R,K,eiland(Q)), (member((R,K,eiland(Q)),B), nonvar(Q)), L).

%hoeveel bruggen kunnen we nog verbinden?
eiland_quota(Bord, (R,K,eiland(Quota)), Vrij) :-
	bruggen_verbonden(Bord,(R,K,eiland(Quota)),_, BruggenAantal),
	Vrij is Quota - BruggenAantal.

% hoeveel bruggen zijn verbonden?
bruggen_verbonden(Bord, (R,K,_), Bruggen, Aantal) :-
	%zoek de buren die bruggen zijn volgens de juiste richting
	findall(brug(h,A), ((K1 is K+1; K1 is K-1),memberchk((R,K1,brug(h,A)),Bord), nonvar(A)), L1),
	findall(brug(v,B), ((R1 is R+1; R1 is R-1),memberchk((R1,K,brug(v,B)),Bord), nonvar(B)), L2),
	append(L1,L2,Bruggen),
	findall(Breedte, member(brug(_,Breedte),Bruggen), Breedtes),
	sumlist(Breedtes,Aantal).

eiland_brug_buren(Eilanden, Map) :-
	findall(Eiland-(BurenLijst,[]), (member(Eiland,Eilanden),eiland_brug_buren(Eiland,Eilanden,BurenLijst)), Lijst),
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
losop_eiland(_,[],_,_).
losop_eiland(Bord, [Eiland1|Rest], Map, MapOut) :-
	losop_eiland(Bord, Eiland1, Map, [Eiland1|Rest], MapOut).
%geen onopgeloste meer
losop_eiland(_, _, _, [],_).
losop_eiland(Bord, Eiland1, Map, Onopgelost, MapOut) :-
	%update_gui(Bord),
	eiland_quota(Bord, Eiland1, RestQuota),
	delete(Onopgelost,Eiland1,Onopgelost1),
	((RestQuota > 0) ->
		( %if then
		get_assoc(Eiland1, Map, (BurenLijst,BruggenLijst)), 
		member(BuurEiland1,BurenLijst),
		memberchk(BuurEiland1,Onopgelost),
		\+memberchk(BuurEiland1,BruggenLijst),
		Eiland1 @< BuurEiland1,
		verbind(Bord,Map,Eiland1,BuurEiland1,Map1),	
		losop_eiland(Bord, Eiland1, Map1, Onopgelost, _)
		); %else RestQuota =:= 0
		(
		losop_eiland(Bord, Onopgelost1, Map, MapOut)
		)
	).

verbonden_eilanden(Map,Eiland,Eiland2) :-
	(get_assoc(Eiland,Map,(BuurEilanden,VerbondenEilanden)), 
	member(Eiland2,VerbondenEilanden))
	;
	
	(member(Eiland3,BuurEilanden),
	%verbonden_eilanden(Map,Eiland2,Eiland3),
	get_assoc(Eiland3,Map,(_,VerbondenEilanden3)), 
	member(Eiland2,VerbondenEilanden3)
	).

bord([  (1,1,eiland(2)),  (1,2,_), (1,3,eiland(2)),
	(2,1,_), 	  (2,2,_), (2,3,_), 
	(3,1,eiland(2)),  (3,2,_), (3,3,eiland(2)) 
	]).		  

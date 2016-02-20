:-ensure_loaded(opgave).
:-ensure_loaded(gui).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
hashi(Bord) :-
	eiland_brug_buren(Bord,Eilanden,Map),
	losop_eiland(Bord, Eilanden,Map).
test(Bord,MoetenDubbele) :-
	bord(Bord),
	eiland_brug_buren(Bord,Eilanden,Map),
	relaxeer(Bord, Eilanden, Map, MoetenDubbele),
	losop_eiland(Bord,Eilanden,Map).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
eiland_brug_buren(Bord, Eilanden, Map) :-
	findall((R1,K1,eiland(Q1)), (member((R1,K1,eiland(Q1)),Bord), nonvar(Q1)), Eilanden),
	
	findall(Eiland-BurenLijst, 
		(member(Eiland,Eilanden),eiland_brug_buren(Eiland, Eilanden, BurenLijst)),
		Lijst),
	list_to_assoc(Lijst,Map).

%een eiland heeft volgende brugburen
eiland_brug_buren((R,K,eiland(Q)), Eilanden , Lijst) :-
	findall((R,K2,eiland(Q2)), member((R,K2,eiland(Q2)),Eilanden), Horizontaal),
	findall((R3,K,eiland(Q3)), member((R3,K,eiland(Q3)),Eilanden), Verticaal),
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
	
relaxeer(_, Eilanden, Map, MoetenDubbele) :-
	findall((R,K,eiland(N)), 
		(member((R,K,eiland(N)),Eilanden), 
			%N mod 2 =:= 0, 
			get_assoc((R,K,eiland(N)), Map, (BurenLijst)), 
			length(BurenLijst,AantalBuren), 
			AantalBuren =:=N/2),
		 MoetenDubbele).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
verbind(Bord, (R,K1,eiland(Q1)),Breedte,(R,K2,eiland(Q2))) :-
	K2-K1 > 1,
	verbind_hor(Bord, (R,K1,eiland(Q1)), Breedte, (R,K2,eiland(Q2))).
verbind(Bord, (R,K1,eiland(Q1)),Breedte,(R,K2,eiland(Q2))) :-
	K1-K2 > 1,
	verbind_hor(Bord, (R,K2,eiland(Q2)), Breedte, (R,K1,eiland(Q1))).
verbind(Bord, (R1,K,eiland(Q1)),Breedte,(R2,K,eiland(Q2))) :-
	R2-R1 > 1,
	verbind_ver(Bord, (R1,K,eiland(Q1)), Breedte, (R2,K,eiland(Q2))).
verbind(Bord, (R1,K,eiland(Q1)),Breedte,(R2,K,eiland(Q2))) :-
	R1-R2 > 1,
	verbind_ver(Bord, (R2,K,eiland(Q2)), Breedte, (R1,K,eiland(Q1))).
verbind_hor(Bord, (R,K1,eiland(Q1)), Breedte, (R,K2,eiland(Q2))) :-
	K is K1+1, memberchk((R,K,Inhoud),Bord), var(Inhoud),
	verbind_hor(Bord, (R,K,eiland(Q1)), Breedte, K, (R,K2,eiland(Q2))).
verbind_hor(_, (R,_,eiland(_)), _, K, (R,K,eiland(_))).
verbind_hor(Bord, (R,K1,eiland(Q1)), Breedte, Acc, (R,K2,eiland(Q2))) :-
	Acc < K2,
	memberchk((R,Acc,brug(h,Breedte)),Bord),
	Acc1 is Acc+1,
	verbind_hor(Bord, (R,K1,eiland(Q1)), Breedte, Acc1, (R,K2,eiland(Q2))).
verbind_ver(Bord, (R1,K,eiland(Q1)), Breedte, (R2,K,eiland(Q2))) :-
	R is R1+1,memberchk((R,K,Inhoud),Bord), var(Inhoud),
	verbind_ver(Bord, (R1,K,eiland(Q1)), Breedte, R, (R2,K,eiland(Q2))).
verbind_ver(_, (_,K,eiland(_)), _, R, (R,K,eiland(_))).
verbind_ver(Bord, (R1,K,eiland(Q1)), Breedte, Acc, (R2,K,eiland(Q2))) :-
	Acc < R2,
	memberchk((Acc,K,brug(v,Breedte)),Bord),
	Acc1 is Acc+1,
	verbind_ver(Bord, (R1,K,eiland(Q1)), Breedte, Acc1, (R2,K,eiland(Q2))).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%hoeveel bruggen kunnen we nog verbinden?
eiland_quota(Bord, (R,K,eiland(Q)), Vrij) :-
	bruggen_verbonden(Bord,(R,K,eiland(Q)), BruggenAantal),
	Vrij is Q - BruggenAantal.

% hoeveel bruggen zijn verbonden?
bruggen_verbonden(Bord, (R,K,eiland(_)), Aantal) :-
	%zoek de buren die bruggen zijn volgens de juiste richting
	findall(A, ((K1 is K+1; K1 is K-1),memberchk((R,K1,brug(h,A)),Bord), nonvar(A)), L1),
	findall(B, ((R1 is R+1; R1 is R-1),memberchk((R1,K,brug(v,B)),Bord), nonvar(B)), L2),
	%append(L1,L2,Bruggen),
	%findall(Breedte, member(brug(_,Breedte),Bruggen), Breedtes),
	sumlist(L1,A1), sumlist(L2,A2),
	Aantal is A1+A2.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%losop_eiland(_,[],_,_).
losop_eiland(Bord, [Eiland1|Rest],Map) :-
	losop_eiland(Bord, Eiland1, [Eiland1|Rest],Map).
%geen onopgeloste meer
losop_eiland(_, [],_).
losop_eiland(_, _, [],_).
losop_eiland(Bord, Eiland1, Onopgelost,Map) :-
	%update_gui(Bord),
	eiland_quota(Bord, Eiland1, RestQuota),
	select(Eiland1,Onopgelost,Onopgelost1),
	((RestQuota > 0) ->
		( %if then
		get_assoc(Eiland1,Map,BuurLijst),
		member(BuurEiland1,BuurLijst),
		Eiland1 @< BuurEiland1,
		memberchk(BuurEiland1,Onopgelost1),
		eiland_quota(Bord, BuurEiland1, RestQuota2),
		RestQuota2 > 0,
		member(Breedte,[2,1]),
		Breedte =< RestQuota, Breedte =< RestQuota2, 
		verbind(Bord,Eiland1,Breedte,BuurEiland1),	
		losop_eiland(Bord, Eiland1, Onopgelost, Map)
		); %else RestQuota =:= 0
		(
		losop_eiland(Bord, Onopgelost1,Map)
		)
	).


bord([  (1,1,eiland(4)),  (1,2,_), (1,3,eiland(2)),
	(2,1,_), 	  (2,2,_), (2,3,_), 
	(3,1,eiland(4)),  (3,2,_), (3,3,eiland(2)) 
	]).
	  

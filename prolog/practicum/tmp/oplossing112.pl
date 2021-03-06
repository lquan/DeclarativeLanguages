:-ensure_loaded(opgave).
:-ensure_loaded(gui).
bord([(1,1,_),(1,2,eiland(4)),(1,3,_),(1,4,_),(1,5,_),(1,6,eiland(4)),
(2,1,_),(2,2,_),(2,3,_),(2,4,_),(2,5,_),(2,6,_),
(3,1,_),(3,2,_),(3,3,_),(3,4,eiland(1)),(3,5,_),(3,6,_),
(4,1,_),(4,2,_),(4,3,_),(4,4,_),(4,5,_),(4,6,_),
(5,1,_),(5,2,eiland(1)),(5,3,_),(5,4,eiland(3)),(5,5,_),(5,6,eiland(2))]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
hashi(Bord) :-
	zoek_eilanden(Bord, Eilanden),
	losop_eiland(Bord, Eilanden).

test(B,Map) :-
	bord(B),
	relaxatie(B,Map).
	%gen_assoc(Key,Map,Value).
	 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%relaxatie(Bord, Map2) :-
%	zoek_eilanden(Bord, Eilanden),
%	eiland_brug_buren(Eilanden, Map),
%	findall((R,K,eiland(N))-Buren, 
%		(gen_assoc((R,K,eiland(N)),Map,Buren), N mod 2 =:= 0, length(Buren, AantalBuren),AantalBuren =:= N/2),  ZekereEilanden),
%	list_to_assoc(ZekereEilanden,Map2).

%relaxeer(Bord,Map) :-
%	relaxatie(Bord,Map),
%	foreach( (gen_assoc(Eiland,Map,Buren), member(Buur,Buren), eiland_quota(Bord, Buur, Rest), Rest > 1), 
%(verbind(Bord, Eiland, Buur),!; verbind(Bord, Buur, Eiland),!)).
	

%maak_twee_bruggen((R,K1,eiland(Q1)), [(R,K2,eiland(Q2))|Rest]) :-
%	K2 > K1 -> ( Kl is K1 + 1, Kh is K2 - 1,
%	foreach(between(K1,K2,K), memberchk( (R,K,brug(h,2))) )
%	;
%	(foreach(between(K2,K1,K), memberchk( (R,K,brug(h,2)))
%	maak_twee_bruggen(
%maakt een associatiemap van elk eiland en zijn mogelijke buren
eiland_brug_buren(Eilanden, Map) :-
	findall( Eiland-BurenLijst, (member(Eiland,Eilanden), eiland_brug_buren(Eiland,Eilanden, BurenLijst)), Lijst),
	list_to_assoc(Lijst,Map).
%een eiland heeft volgende brugburen
eiland_brug_buren((R,K,eiland(Q)), Eilanden, Lijst) :-
	findall( (R,K1,eiland(Q1)), member( (R,K1,eiland(Q1)), Eilanden), Horizontaal),
	findall( (R1,K,eiland(Q2)), member( (R1,K,eiland(Q2)), Eilanden), Verticaal),
	
	findall( (R, K3, eiland(Q3)), 
( (nextto((R,K,eiland(Q)), (R,K3,eiland(Q3)), Horizontaal), K3 - K > 1); nextto((R, K3, eiland(Q3)), (R,K,eiland(Q)), Horizontaal)), Hor),
	findall( (R4, K, eiland(Q4)), (nextto((R,K,eiland(Q)), (R4,K,eiland(Q4)), Verticaal); nextto((R4, K, eiland(Q4)), (R,K,eiland(Q)), Verticaal)), Ver),
	append(Hor,Ver,Lijst).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
verbind(Bord, (R1,K1,eiland(Q1)), (R2,K2,eiland(Q2))) :-
	eiland_quota(Bord,(R2,K2,eiland(Q2)),Vrij),
	( Vrij =:= 1 -> 
		(verbind_hor(Bord, (R1,K1,eiland(Q1)), 1, (R2,K2,eiland(Q2))) ;
		verbind_ver(Bord, (R1,K1,eiland(Q1)), 1, (R2,K2,eiland(Q2))))
;
	Vrij > 1, (verbind_hor(Bord, (R1,K1,eiland(Q1)), 2, (R2,K2,eiland(Q2))) ;
		verbind_hor(Bord, (R1,K1,eiland(Q1)), 1, (R2,K2,eiland(Q2)));
		verbind_ver(Bord, (R1,K1,eiland(Q1)), 2, (R2,K2,eiland(Q2))) ;
		verbind_ver(Bord, (R1,K1,eiland(Q1)), 1, (R2,K2,eiland(Q2))))
	) .

%%%%%%%%%%%%%%%%%%%%%%%%%%%horizontale brug
verbind_hor(Bord, (R,K1,eiland(Q1)), Breedte, (R,K2,eiland(Q2))) :-
	K2-K1 > 1,
	K is K1+1,
	memberchk((R,K,I),Bord), var(I), %%vrij
	verbind_hor(Bord, (R,K1,eiland(Q1)), K, Breedte, (R,K2,eiland(Q2))).

%%%de accumulator houdt bij waar we zijn (rechts van het eerste eiland)
%%maak brug en ga naar rechts...
verbind_hor(Bord, (R,K1,eiland(Q1)), Acc, Breedte, (R,K2,eiland(Q2))) :-
	Acc < K2,
	memberchk((R,Acc,brug(h,Breedte)),Bord),
	Acc1 is Acc+1,	
	verbind_hor(Bord, (R,K1,eiland(Q1)), Acc1, Breedte, (R,K2,eiland(Q2))).
%%%...totdat we hebben het eiland bereikt hebben. (Acc = K2)
verbind_hor(_, (R,_,_), K2, _, (R,K2,_)).

%%%%%%%%%%%%%%%%%%analoog voor verticale bruggen
verbind_ver(Bord, (R1,K,eiland(Q1)), Breedte, (R2,K,eiland(Q2))) :-
	R2-R1 > 1,
	R is R1+1,
	memberchk((R,K,I), Bord), var(I),
	verbind_ver(Bord, (R1,K,eiland(Q1)), R, Breedte, (R2,K,eiland(Q2))).

verbind_ver(_, (_,K,_), R2, _, (R2,K,_)).
verbind_ver(Bord, (R1,K,eiland(Q1)), Acc, Breedte, (R2,K,eiland(Q2))) :-
	Acc < R2,
	memberchk( (Acc,K,brug(v,Breedte)),Bord),
	Acc1 is Acc+1,	
	verbind_ver(Bord, (R1,K,eiland(Q1)), Acc1,Breedte, (R2,K,eiland(Q2))).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%zoek de eilanden
zoek_eilanden(B,L) :-
	findall((R,K,eiland(Q)), (member((R,K,eiland(Q)),B), nonvar(Q)), L).

%hoeveel bruggen kunnen we nog verbinden?
eiland_quota(Bord, (R,K,eiland(Quota)), Rest) :-
	aantal_bruggen(Bord,(R,K,eiland(Quota)),Bruggen),
	Rest is Quota - Bruggen.

% hoeveel bruggen zijn verbonden?
aantal_bruggen(Bord, (R,K,_), Aantal) :-
	%zoek de buren die bruggen zijn
	findall(A, ((K1 is K + 1; K1 is K-1),memberchk((R,K1,brug(h,A)),Bord), nonvar(A)), L1),
	findall(B, ((R1 is R + 1; R1 is R-1),memberchk((R1,K,brug(v,B)),Bord), nonvar(B)), L2),
	sumlist(L1,A1),
	sumlist(L2,A2),
	Aantal is A1 + A2.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%initialisatie	
losop_eiland(Bord, [Eiland1|EilandRest]) :-
	losop_eiland(Bord, Eiland1, EilandRest).
%geen onopgeloste meer
losop_eiland(_, _, []).
losop_eiland(Bord, E1, [E2]) :-
	eiland_quota(Bord, E1, 0),
	eiland_quota(Bord, E2, 0).
%los het gegeven eiland op gegeven de lijst van de andere onopgeloste eilanden
losop_eiland(Bord, (R1,K1,eiland(Q1)), [Onopgelost,Onopgelost1|Rest]) :-
	update_gui(Bord),
	eiland_quota(Bord, (R1,K1,eiland(Q1)), RestQuota),
	(RestQuota =:= 0 ->
		(delete([Onopgelost,Onopgelost1|Rest], (R1,K1,eiland(Q1)), [OnopgelostNieuw|Tail]),
		losop_eiland(Bord, OnopgelostNieuw, [OnopgelostNieuw|Tail]))
		;
		(member(AnderEiland,[Onopgelost,Onopgelost1|Rest]),
		%AnderEiland \== (R1,K1,eiland(Q1)),	
		verbind(Bord, (R1,K1,eiland(Q1)), AnderEiland),
		losop_eiland(Bord, (R1,K1,eiland(Q1)), [Onopgelost,Onopgelost1|Rest]))
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

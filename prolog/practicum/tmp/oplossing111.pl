:-ensure_loaded(opgave).
:-ensure_loaded(gui).
bord([(1,1,_),(1,2,eiland(1)),(1,3,_),(1,4,_),(1,5,_),(1,6,eiland(2)),
(2,1,_),(2,2,_),(2,3,_),(2,4,_),(2,5,_),(2,6,_),
(3,1,_),(3,2,_),(3,3,_),(3,4,eiland(1)),(3,5,_),(3,6,_),
(4,1,_),(4,2,_),(4,3,_),(4,4,_),(4,5,_),(4,6,_),
(5,1,_),(5,2,eiland(1)),(5,3,_),(5,4,eiland(3)),(5,5,_),(5,6,eiland(2))]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
hashi(Bord) :-
	zoek_eilanden(Bord, Eilanden),
	losop_eiland(Bord, Eilanden).

test(B) :-
	bord(B),
	hashi(B).
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
	findall(A, (hor_buur((R,K),(_,_,brug(h,A)),Bord), nonvar(A)), L1),
	findall(B, (ver_buur((R,K),(_,_,brug(v,B)),Bord), nonvar(B)), L2),
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
	%update_gui(Bord),
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%buurpredicaten
linkerbuur((R,K),(R,K1,I),B) :-
	K > 1,
	K1 is K - 1,
	memberchk((R,K1,I),B).
rechterbuur((R,K),(R,K1,I),B) :-
	K1 is K + 1,
	memberchk((R,K1,I),B).
bovenbuur((R,K),(R1,K,I),B) :-
	R > 1,
	R1 is R - 1,
	memberchk((R1,K,I),B).
onderbuur((R,K),(R1,K,I),B) :-
	R1 is R + 1,
	memberchk((R1,K,I),B).

hor_buur((R,K),(R1,K1,I),Bord) :-
	linkerbuur((R,K),(R1,K1,I),Bord);
	rechterbuur((R,K),(R1,K1,I),Bord).

ver_buur((R,K),(R1,K1,I),Bord) :-
	bovenbuur((R,K),(R1,K1,I),Bord);
	onderbuur((R,K),(R1,K1,I),Bord).


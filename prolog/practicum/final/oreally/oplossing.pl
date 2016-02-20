% Li Quan
% 1e master cw optie ai
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:-ensure_loaded(opgave).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
hashi(Bord) :-
	maak_map(Bord, Eilanden, Map),	
	losop_eiland(Bord, Eilanden, Map).	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
zoek_eilanden(Bord, L) :- findall((R,K,eiland(Q)), (member((R,K,eiland(Q)),Bord),nonvar(Q)), L).
%maak een map van de eilanden en hun initiele buren zodat we enkel beperkt moeten zoeken.
maak_map(Bord, Eilanden, Map) :-
	zoek_eilanden(Bord, Eilanden),	
	findall(Eiland-(InitieleBurenLijst), (member(Eiland,Eilanden),zoek_buren(Bord, Eilanden, Eiland, InitieleBurenLijst)), Lijst),
	list_to_assoc(Lijst,Map).
%zoek de mogelijke eilanden waar we een brug naartoe kunnen bouwen gegeven de eilandenlijst	
% dit zijn maximaal 2 elementen enkel van linksboven, naar rechtsonder
zoek_buren(Bord, Eilanden, (R,K,eiland(Q)), BuurLijst) :-
	findall((R,K2,eiland(Q2)), member((R,K2,eiland(Q2)),Eilanden), Horizontaal),
	findall((R3,K,eiland(Q3)), member((R3,K,eiland(Q3)),Eilanden), Verticaal),
	findall((R4,K4,eiland(Q4)), (member((R4,K4,eiland(Q4)),Eilanden), 
					(nextto((R,K,eiland(Q)),(R4,K4,eiland(Q4)), Horizontaal); nextto((R,K,eiland(Q)),(R4,K4,eiland(Q4)), Verticaal))),
			BuurLijst).		
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%VERBINDINGEN
%horizontale verbinding
verbind(Bord, (R,K1,eiland(Q1)), Vrij1, Breedte, (R,K2,eiland(Q2)), Vrij2) :-
	kan_verbonden_zijn((R,K1,eiland(Q1)),Vrij1,Breedte,(R,K2,eiland(Q2)), Vrij2), 
	K is K1+1,
	verbind(Bord, (R,K1,eiland(Q1)), Breedte, K, (R,K2,eiland(Q2))).
%verticale verbinding
verbind(Bord, (R1,K,eiland(Q1)), Vrij1, Breedte, (R2,K,eiland(Q2)), Vrij2) :-
	kan_verbonden_zijn((R1,K,eiland(Q1)),Vrij1,Breedte,(R2,K,eiland(Q2)), Vrij2),
	R is R1+1,
	verbind(Bord, (R1,K,eiland(Q1)), Breedte, R, (R2,K,eiland(Q2))).
verbind(_, (R,_,eiland(_)), _, K2, (R,K2,eiland(_))).
verbind(_, (_,K,eiland(_)), _, R2, (R2,K,eiland(_))).
verbind(Bord, (R,K1,eiland(Q1)), Breedte, Acc, (R,K2,eiland(Q2))) :-
	Acc < K2, 
	memberchk((R,Acc,brug(h,Breedte)),Bord), 
	Acc1 is Acc+1,
	verbind(Bord, (R,K1,eiland(Q1)), Breedte, Acc1, (R,K2,eiland(Q2))).
verbind(Bord, (R1,K,eiland(Q1)), Breedte, Acc, (R2,K,eiland(Q2))) :-
	Acc < R2, 
	memberchk((Acc,K,brug(v,Breedte)),Bord),
	Acc1 is Acc+1,
	verbind(Bord, (R1,K,eiland(Q1)), Breedte, Acc1, (R2,K,eiland(Q2))).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% VERBINDING EN QUOTA CHECKS
kan_verbonden_zijn((R1,K1,eiland(Q1)),Vrij1, Breedte, (R2,K2,eiland(Q2)), Vrij2) :-
	(R1,K1,eiland(Q1)) @< (R2,K2,eiland(Q2)),
	Vrij1 > 0, Vrij2 > 0,
	Breedte =< Vrij1,
	Breedte =< Vrij2,
	(Q1 =:= 1 -> Q2 > 1; 
		((Q1 =:= 2, Q2 =:= 2) -> (Breedte =\= 2);
			 Breedte =< 2)).		 		     
eiland_quota(Bord, (R,K,eiland(Q)), Vrij) :- 
	bruggen_verbonden(Bord,(R,K,eiland(Q)), BruggenAantal), 
	Vrij is Q - BruggenAantal.	
bruggen_verbonden(Bord, (R,K,eiland(_)), Aantal) :-
	findall(A, ((K1 is K+1; K1 is K-1),memberchk((R,K1,brug(h,A)),Bord), nonvar(A)), L1),
	findall(B, ((R1 is R+1; R1 is R-1),memberchk((R1,K,brug(v,B)),Bord), nonvar(B)), L2),
	sumlist(L1,A1), sumlist(L2,A2),
	Aantal is A1+A2.		
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%CODE VOOR VERBONDENHEID TE CHECKEN
%maak een map van elk eiland met de lijst van andere eilanden naar waar hij verbonden is	
maak_map_verbindingen(Bord, Eilanden, Map) :-
	findall(Eiland-Verbonden, (member(Eiland,Eilanden),zoek_verbonden(Bord,Eilanden,Eiland,Verbonden)), Lijst),
	list_to_assoc(Lijst,Map).
%zoek de lijst van eilanden verbondenen met het gegeven eiland
zoek_verbonden(Bord, Eilanden, Eiland, Lijst) :-
	delete(Eilanden,Eiland,R),
	findall(Eiland2, (member(Eiland2,R),verbonden(Bord,Eiland,Eiland2)), Lijst).
%een eiland is verbonden met een ander eiland als ...		
verbonden(Bord, (R,K1,eiland(_)), (R,K2,eiland(_))) :-
	K2-K1 > 1, 
	K3 is K1+1, 
	K4 is K2-1,
	forall(between(K3,K4,K), (memberchk((R,K,brug(h,A)),Bord), nonvar(A))).	
verbonden(Bord, (R,K1,eiland(Q1)), (R,K2,eiland(Q2))) :-
	K1-K2 > 1, 
	verbonden(Bord, (R,K2,eiland(Q2)), (R,K1,eiland(Q1))).
verbonden(Bord, (R1,K,eiland(_)), (R2,K,eiland(_))) :-
	R2-R1 > 1, 
	R3 is R1+1, 
	R4 is R2-1,
	forall(between(R3,R4,R), (memberchk((R,K,brug(v,A)),Bord), nonvar(A))).
verbonden(Bord, (R1,K,eiland(Q1)), (R2,K,eiland(Q2))) :-
	R1-R2 > 1, 
	verbonden(Bord, (R2,K,eiland(Q2)), (R1,K,eiland(Q1))).	
%er bestaat een pad van eiland1 naar eiland2 als...				
pad(Eiland1, Eiland2, _, Map) :- 
	get_assoc(Eiland1, Map, Verbonden),
	member(Eiland2, Verbonden), !.    
pad(Eiland1, Eiland2, P, Map) :- 
    get_assoc(Eiland1, Map, Verbonden),
    member(Eiland3, Verbonden),
    \+ member(Eiland3, P), 
    pad(Eiland3,Eiland2,[Eiland3|P], Map).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%HET OPLOSSEN ZELF
%initialisatie los op
losop_eiland(Bord, [Eiland1|Rest], Map) :-
	losop_eiland(Bord, Eiland1, Rest, Map).
%geen onopgeloste meer, check verbondenheid.
losop_eiland(Bord, [], _) :- 
	zoek_eilanden(Bord, [Eiland1|Rest]),
	maak_map_verbindingen(Bord, [Eiland1|Rest], Map),
 	forall(member(AnderEiland,Rest), pad(Eiland1, AnderEiland, [], Map)).
%los eiland 1 op en los dan de andere op: we lossen op van links naar rechts, boven naar onder
losop_eiland(Bord, Eiland1, Onopgelost, Map) :-
	eiland_quota(Bord, Eiland1, RestQuota1),
	(RestQuota1 > 0 ->
		( %RestQuota1 > 0, Eiland 1 nog niet opgelost
		get_assoc(Eiland1, Map, BuurLijst),
		%zoek_buren(Bord, MogelijkeBurenInitieel, Eiland1, BuurLijst),
		select(BuurEiland1,BuurLijst,BuurLijst1), 
		eiland_quota(Bord, BuurEiland1, RestQuota2), 
		member(Breedte,[2,1]), 
		verbind(Bord,Eiland1,RestQuota1,Breedte,BuurEiland1,RestQuota2),
		RestQuota1Nieuw is RestQuota1 - Breedte,
		RestQuota2Nieuw is RestQuota2 - Breedte,
		(RestQuota1Nieuw > 0 ->
		      (%we moeten nu Eiland 1 oplossen dus moet kleiner dan 2 zijn aangezien we maar 1 extra brug kunnen plaatsen
		      RestQuota1Nieuw =< 2, 
		      member(BuurEiland2,BuurLijst1), BuurEiland1 @< BuurEiland2,
		      eiland_quota(Bord, BuurEiland2, RestQuota3), 
		      verbind(Bord,Eiland1,RestQuota1Nieuw,RestQuota1Nieuw,BuurEiland2,RestQuota3), 
		      losop_eiland(Bord,Onopgelost,Map)
		      )
		      ; %end if RestQuota1Nieuw
		      (%RestQuota1Nieuw =:=0, Eiland1 is opgelost ...
		      (RestQuota2Nieuw > 0 -> losop_eiland(Bord,Onopgelost,Map) %... maar BuurEiland1 nog niet,
			  ; 
			  (delete(Onopgelost,BuurEiland1,Onopgelost1), %... en BuurEiland1 ook.
			  losop_eiland(Bord,Onopgelost1,Map)))
		      )
	        ))
        	; %else RestQuota1 =:= 0
		losop_eiland(Bord,Onopgelost,Map)).

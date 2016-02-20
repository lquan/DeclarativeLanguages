% Li Quan
% 1e master cw
% practicum
:- ensure_loaded(opgave).
bord([ (1,1,eiland(1)), (1,2,_), (1,3,_), (1,4,eiland(2)), (1,5,_),(1,6,eiland(1)) ]).

hashi(Bord) :-
	dimensies_bord(Rijen,Kolommen,Bord),
        zoek_vrij(Bord,Vrij),
	findall(Vulling, vul_leeg(Vulling), Vullingen),
	hashi_recurse(Bord, Rijen, Kolommen, Vullingen, Vrij).
	

hashi_recurse(Bord,_,_,_,[]) :-
	zoek_eilanden(Bord,Eilanden),
	forall(member((R,K,Eiland),Eilanden), quota_juist((R,K,Eiland), Bord)).
hashi_recurse(B,Rijen,Kolommen,Vullingen,[(R,K)|Tail]) :-
	zoek_buren((R,K),B,Buren),
	%quota van bovenbuur moet perfect zijn
	%findall((R1,K1,I1), (bovenbuur((R,K),(R1,K1,I1),B), nonvar(I1), I1 = eiland(_)), BovenBuur),
	
	member(V, Vullingen),

	forall(	(member((R1,K1,BuurInhoud),Buren), nonvar(BuurInhoud)),

		(mogelijke_verbinding(BuurInhoud,V),
		mogelijk(V, (R,K), (Rijen,Kolommen)), %directionaliteit
		quota_ok(V,(R1,K1,BuurInhoud),Buren))
	),
	%bovenbuur((R,K),(R1,K,I),B)
	memberchk((R,K,V),B),
	hashi_recurse(B,Rijen,Kolommen,Vullingen,Tail).

%aantal rijen en kolommen van het bord
dimensies_bord(R,K,Bord) :-
	last(Bord,(R,K,_)).

quota_ok(_,(_,_,brug(_,_)), _).
quota_ok(V, (_,_,eiland(_)), _) :-
	var(V).
quota_ok(brug(_,Breedte), (R,K,eiland(Quota)), Buren) :-
	aantal_bruggen((R,K),Buren,Aantal),
	Aantal + Breedte =< Quota.

quota_juist((R,K,eiland(Q)), B) :-
	zoek_buren((R,K),B,Buren),
	%zoek de buren die bruggen zijn
	findall(A, (member((_,_,I1),Buren), nonvar(I1), I1 = brug(_,A)), L),
	sumlist(L,Q).
	%aantal_bruggen((R,K),B,Q).

% hoeveel bruggen zijn verbonden aan (R,K) gegeven zijn Buren?
aantal_bruggen((R,K),Buren,Aantal) :-
	%zoek de buren die bruggen zijn
	findall(A, (member((R,K,I1),Buren), nonvar(I1), I1 = brug(_,A)), L),
	sumlist(L,Aantal).

% wat moeten we invullen?
vul_leeg(brug(h,1)).
vul_leeg(brug(h,2)).
vul_leeg(brug(v,2)).
vul_leeg(brug(v,1)).
vul_leeg(X) :- var(X).

%eiland kan zeker naast leeg staan
mogelijke_verbinding(brug(Richting,Breedte), brug(Richting,Breedte)).

%..of naast een brug (rekening houdend met quota)
mogelijke_verbinding(eiland(Quota), X) :- nonvar(X), X = brug(_,Breedte), Quota >= Breedte.
%mogelijke_verbinding(eiland(Quota), X) :- nonvar(X), X = brug(v,Breedte), Quota >= Breedte.
%brug moet met zelfde soort brug verbonden zijn
mogelijke_verbinding(eiland(_), X) :- var(X).


%de mogelijkheden ivm de rij en kolommen voor de bruggen
mogelijk(X, (_,_), (_,_)) :- var(X), !.
mogelijk(brug(h,_), (_,K), (_, Kolommen)) :-
	K > 1,
	K < Kolommen.
mogelijk(brug(v,_), (R,_), (Rijen,_)) :-
	R > 1,
	R < Rijen.

%buurpredicaten
linkerbuur((R,K),(R,K1,I),B) :-
	K1 is K -1,
	memberchk((R,K1,I),B).
rechterbuur((R,K),(R,K1,I),B) :-
	K1 is K+1,
	memberchk((R,K1,I),B).
bovenbuur((R,K),(R1,K,I),B) :-
	R1 is R-1,
	memberchk((R1,K,I),B).
onderbuur((R,K),(R1,K,I),B) :-
	R1 is R+1,
	memberchk((R1,K,I),B).

buur((R,K),(R,K1,I),Bord) :-
	linkerbuur((R,K),(R1,K1,I),Bord);
	rechterbuur((R,K),(R1,K1,I),Bord);
	bovenbuur((R,K),(R1,K1,I),Bord);
	onderbuur((R,K),(R1,K1,I),Bord).

%zoek buren van vakje (R,K) (die niet leeg zijn)
zoek_buren((R,K),B,L) :-
	findall((R1,K1,I1), (buur((R,K), (R1,K1,I1), B)), L).

%zoek de (R,K) van de ongebonden variabelen
zoek_vrij(B,L) :-
	findall((R,K), (member((R,K,X),B),var(X)), L).

%zoek de (R,K) van de eilanden
zoek_eilanden(B,L) :-
	findall((R,K,X), (member((R,K,X),B),nonvar(X),X=eiland(_)), L).


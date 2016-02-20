% Li Quan
% 1e master cw
% practicum
:- ensure_loaded(opgave).
bord([ (1,1,eiland(1)), (1,2,_), (1,3,_), (1,4,eiland(1)) ]).

hashi(Bord) :-
	dimensies_bord(Rijen,Kolommen,Bord),
        zoek_vrij(Bord,Vrij),
	findall(Vulling, vul_leeg(Vulling), Lijst),
	forall((member((R,K),Vrij),
(	member(V, Lijst), 
	mogelijk(V, (R,K), (Rijen,Kolommen)),
	findall((R1,K1,I1), (buur((R,K), (R1,K1,I1), Bord)), Buren),
 	forall(member((_,_,BuurInhoud),Buren), mogelijke_verbinding(BuurInhoud,V)),
	
	memberchk((R,K,V),Bord)
))).


%aantal rijen en kolommen van het bord
dimensies_bord(R,K,Bord) :-
	last(Bord,(R,K,_)).


% wat moeten we invullen?
vul_leeg(brug(h,1)).
vul_leeg(brug(h,2)).

vul_leeg(brug(v,2)).
vul_leeg(brug(v,1)).
vul_leeg(X) :- var(X).

%brug moet met zelfde soort brug verbonden zijn
mogelijke_verbinding(brug(Richting,Breedte), brug(Richting,Breedte)).
mogelijke_verbinding(eiland(Quota), brug(_,Breedte)) :- Quota >= Breedte.
mogelijke_verbinding(eiland(_), X) :- var(X).
%mogelijke_verbinding(X, X) :- var(X).

mogelijk(brug(h,_), (_,K), (_, Kolommen)) :-
	K > 1,
	K < Kolommen.
mogelijk(brug(v,_), (R,_), (Rijen,_)) :-
	R > 1,
	R < Rijen.
mogelijk(X, (_,_), (_,_)) :-
	var(X).

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

buur((R,K),(R1,K1,I),Bord) :-
	linkerbuur((R,K),(R1,K1,I),Bord);
	rechterbuur((R,K),(R1,K1,I),Bord);
	bovenbuur((R,K),(R1,K1,I),Bord);
	onderbuur((R,K),(R1,K1,I),Bord).


% hoeveel bruggen zijn verbonden?
aantal_bruggen((R,K),Bord,Aantal) :-
	findall(A, (buur((R,K),(_,_,I1),Bord), nonvar(I1), I1 = brug(_,A)), L),
	sumlist(L,Aantal).

%zoek de (R,K) van de ongebonden variabelen
zoek_vrij(B,L) :-
	findall((R,K), (member((R,K,X),B),var(X)), L).



% Li Quan
% 1e master cw
% practicum
:- ensure_loaded(opgave).
bord([ (1,1,eiland(1)), (1,2,_), (1,3,_), (1,3,eiland(1)) ]).

hashi(B) :-
	zoek_vrij(B,L),
	member((R,K),L),
	genereer((R,K),B,L1),
	member(X,L1),
	memberchk((R,K,X),B).
	%foreach( (member((R,K),L)),
	%(genereer((R,K),B,L1),
	%member(X,L1),
	%(memberchk((R,K,X),B)))).

	
%genereer mogelijkheden voor een leeg vakje
genereer((R,K),B,L) :-
	%var(X),
	zoek_buren(R,K,B,Buren),
	%zoek alle buren op
	%en de mogelijke vullingen
	findall(Vulling, 

	(member((_,_,Buur), Buren),
	 nonvar(Buur), vul_leeg(Vulling), mogelijke_verbinding(Vulling,Buur), mogelijke_brug((R,K,Vulling),B ) ),L1),
	 list_to_set(L1, L).

% wat moeten we invullen?
vul_leeg(brug(h,2)).
vul_leeg(brug(h,1)).
vul_leeg(brug(v,2)).
vul_leeg(brug(v,1)).
%vul_leeg(X) :- var(X).

%mogelijke verbindingen
mogelijke_verbinding(brug(Richting,Breedte), brug(Richting,Breedte)).
mogelijke_verbinding(eiland(Quota), brug(_,Breedte)) :- Quota >= Breedte.
mogelijke_verbinding(brug(_,Breedte), eiland(Quota)) :- Quota >= Breedte.




% hoeveel bruggen zijn verbonden?
aantal_bruggen((R,K),B,A) :-
	zoek_horiz_buren(R,K,B,HorBuren),
	findall(W1, (member((_,_,I1),HorBuren), nonvar(I1), I1 = brug(h,W1)), H1),
	zoek_vert_buren(R,K,B,VerBuren),
	findall(W2, (member((_,_,I2),VerBuren), nonvar(I2), I2 = brug(v,W2)), H2),
	sumlist(H1,A1),
	sumlist(H2,A2),
	A is A1+A2. 
	
% de buren van rij R en kolom K van bord B
zoek_buren(R,K,B,Lijst) :-
	zoek_horiz_buren(R,K,B,Lijst1),
	zoek_vert_buren(R,K,B,Lijst2),
	append(Lijst1,Lijst2,Lijst).

% de horizontale buren		
zoek_horiz_buren(R,K,B,Lijst) :-
	findall((R,K1,I), 
		((K1 is K-1 ; K1 is K+1), memberchk((R,K1,I),B)),
		Lijst).
% de verticale buren			
zoek_vert_buren(R,K,B,Lijst) :-
	findall((R1,K,I), 
		((R1 is R-1 ; R1 is R+1), memberchk((R1,K,I),B)),
		Lijst).

%zoek de (R,K) van de ongebonden variabelen
zoek_vrij(B,L) :-
	findall((R,K),
	(member((R,K,X),B), var(X)),
	L).

%aantal rijen van het bord
aantal_rijen(B, R) :- 
	last(B,(R,_,_)). 
%aantal kolommen
aantal_kolommen(B, K) :- 
	last(B,(_,K,_)).

% kan een brug met deze waarde voorkomen op een bepaalde 
mogelijke_brug((R,_,brug(v,_)),B ) :-
	R > 1, aantal_rijen(B, A), R < A.

mogelijke_brug((_,K,brug(h,_)),B ) :-
	K > 1, aantal_kolommen(B, A), K < A.


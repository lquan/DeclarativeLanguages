% Li Quan
% 1e master cw
% practicum
:- ensure_loaded(opgave).

bord([ (1,1,eiland(1)), (1,2,_), (1,3,_), (1,3,eiland(1)) ]).

%verbonden((R1,K1,eiland(_)), ((R2,K2,eiland(_))), B) :-
zoek_vrij(B,L) :-
	findall((R,K),
	(member((R,K,X),B), var(X)),
	L).

zoek_eiland(R,B,L) :-
	findall((R,K,X), 
		(nonvar(X), X = eiland(_), member((R,K,X),B) ),
		L).

aantal_rijen(B, R) :- 
	last(B,(R,_,_)). 
aantal_kolommen(B, K) :- 
	last(B,(_,K,_)).

% kan een brug met deze waarde voorkomen
brug_leeg((R,_,brug(v,_)),B ) :-
	R > 1, aantal_rijen(B, A), R < A.

brug_leeg((_,K,brug(h,_)),B ) :-
	K > 1, aantal_kolommen(B, A), K < A.

%hashi(B) :-
	
	%memberchk((1,2,brug(h,1)),B),
	%memberchk((2,1,brug(v,1)),B),
	%memberchk((2,3,brug(v,1)),B),
	%memberchk((3,2,brug(h,1)),B).
		
isEiland(X,Y,B) :-  member((X,Y,I),B), nonvar(I), I= eiland(_).

% met wat kan een leeg vakje gevuld worden?
vul_leeg(brug(h,2)).
vul_leeg(brug(h,1)).
vul_leeg(brug(v,2)).
vul_leeg(brug(v,1)).
vul_leeg(X) :- var(X).

geschikt((X,Y,I),B):-
	var(I).

%geschikt((X,Y,I),B) :-
%	nonvar(I), I = brug(h,W),
	
	%zoek_horiz_buren(R,K,B,Lijst),
	%foreach(member((R1,K1,I1), Lijst), (var(I1) ; nonvar(I1), I1 = I)). 	
%nonvar(I), I = brug(h,W),
	%Y1 is Y-1,
	%memberchk((X,Y1,Z),B), 
	%nonvar(Z), (Z = eiland(_); Z = brug(h,W)).
	

%brug_verbonden((X,Y,brug(h,1)), B) :-
%	X1 is X+1,
%	\+ memberchk((X1,Y,brug(h,1)),B), \+ isEiland(X1,Y,B).
%brug_verbonden((X,Y,brug(h,2)), B) :-
%	X1 is X+1,
%	\+ memberchk((X1,Y,brug(h,2)),B), \+ isEiland(X1,Y,B).

%brug_verbonden((X,Y,brug(h,1)), B) :-
%	Y1 is Y+1,
%	\+ memberchk((X,Y1,brug(h,1)),B), \+ isEiland(X,Y1,B).
%brug_verbonden((X,Y,brug(v,2)), B) :-
%	Y1 is Y+1,
%	\+ memberchk((X,Y1,brug(v,2)),B), \+ isEiland(X,Y1,B).

% wanneer kan een leeg vakje met een brug gevuld worden?
%vul_leeg_brug((R,K),B) :-
	

% een brug moet als buur een eiland hebben of een zelfde soort brug
%brug_buur(brug(_,_), eiland(_)).
%brug_buur(brug(h,W),brug(h,W)). 
%brug_buur(brug(v,W),brug(v,W)). 


% hoeveel bruggen zijn aan dit eiland verbonden?
eiland_bruggen((R,K,eiland(_)),B,A) :-
	zoek_horiz_buren(R,K,B,HorBuren),
	findall(W1, (member((_,_,I1),HorBuren), nonvar(I1), I1 = brug(h,W1)), H1),
	zoek_vert_buren(R,K,B,VerBuren),
	findall(W2, (member((_,_,I2),VerBuren), nonvar(I2), I2 = brug(v,W2)), H2),
	sumlist(H1,A1),
	sumlist(H2,A2),
	A is A1+A2. 
	
% de buren van rij R en kolom K van bord B
%zoek_buren(R,K,B,Lijst) :-
%	zoek_horiz_buren(R,K,B,Lijst1),
%	zoek_vert_buren(R,K,B,Lijst2),
%	Lijst = [Lijst1|Lijst2].

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




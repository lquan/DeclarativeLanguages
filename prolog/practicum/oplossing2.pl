% Li Quan
% 1e master cw
% practicum
:- ensure_loaded(opgave).

bord(B) :- 
	B = [ (1,1,eiland(1)), (1,2,_), (1,3,eiland(1)) ].	

	%B = [ (1,1,eiland(2)), (1,2,_), (1,3,eiland(2)), 
	%	  (2,1,_), (2,2,_), (2,3,_), 
	%	  (3,1,eiland(2)), (3,2,_), (3,3,eiland(2))
	%	 ].
	%B = [ (1,1,eiland(2)), (1,2,brug(h,1)), (1,3,eiland(2)), 
	%	  (2,1,brug(v,1)), (2,2,_), (2,3,brug(v,1)), 
	%	  (3,1,eiland(2)), (3,2,brug(h,1)), (3,3,eiland(2))
	%	 ].

hashi(B) :-
	L = B;
	%findall((R,K,I), (member((R,K,I),B), var(I)), L),
	%foreach( member((R,K,I),L), memberchk((R,K,brug(h,1)),B)). 	
	foreach( member((R,K,I),L), correct((R,K,I),B)).
	%foreach(member((R,K,eiland(X)),B), correct((R,K,eiland(X)),B)).
	%foreach(member((R,K,I),B), correct((R,K,I),B)).

% het vakje met R,K,I (waarbij I een vrije variabele is) is juist.
correct((_,_,I),_) :-
	%memberchk((R,K,I),B),
	var(I).

% het vakje met R,K,I (waarbij I een eiland is) is juist.
correct((R,K,I),B) :-
	%memberchk((R,K,I),B),
	nonvar(I) ,  I = eiland(A),
	eiland_bruggen((R,K,I),B,A).

% het vakje met R,K,I (waarbij I een brug is) is juist.
correct((R,K,I),B) :-
	%memberchk((R,K,I),B),
	nonvar(I), I = brug(h,_),
	zoek_horiz_buren(R,K,B,HorBuren),
	foreach( member( (R1,K1,I1),HorBuren), 

(brug_buur(I, I1),memberchk((R1,K1,I1),B))).

correct((R,K,I),B) :-
	%memberchk((R,K,I),B),
	nonvar(I), I = brug(v,_),
	zoek_vert_buren(R,K,B,VerBuren),
	foreach(member( (R1,K1,I1),VerBuren), (brug_buur(I, I1),memberchk((R1,K1,I1),B))).


brug_buur(brug(_,1), (_,_,eiland(_))).
brug_buur(brug(_,2), (_,_,eiland(_))).
brug_buur(brug(h,W),brug(h,W)). 
brug_buur(brug(v,W),brug(v,W)). 	
	
	
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
zoek_buren(R,K,B,Lijst) :-
	zoek_horiz_buren(R,K,B,Lijst1),
	zoek_vert_buren(R,K,B,Lijst2),
	Lijst = [Lijst1|Lijst2].

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
		

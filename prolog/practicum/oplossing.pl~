% Li Quan
% 1e master cw
% practicum
:- ensure_loaded(opgave).

hashi(Bord) :-
	zoek_vrij(B,Onbekend),
	zoek_eiland(B,Eiland).
	
eiland_bereikbaar(B) :-
		zoek_eiland(B,Eilanden),
		
bord(B) :- 
	B = [ (1,1,eiland(2)), (1,2,_), (1,3,eiland(2)), 
		  (2,1,_),         (2,2,_), (2,3,_), 
		  (3,1,eiland(2)), (3,2,_), (3,3,eiland(2))
		 ].

zoek_eiland(B,L) :-
	findall((R,K,X), 
		 (member((R,K,X),B) , nonvar(X), X = eiland(_)),
		 L).
 
 
zoek_vrij(B,L) :-
	findall((R,K),
			(member((R,K,X),B), var(X)),
			L).
%zoek_eiland(R,B,L) :-
%	findall((R,K,X), 
%			(nonvar(X), X = eiland(_), member((R,K,X),B) ),
%			L).

%easy_rule(B,B1) :-
%	zoek_eiland(B,Eilanden),
%	member((R,K,eiland(N1)),Eilanden),
%%	zoek_buren(R,K,B,Buren),
%	listlength(Buren,N2)
	
	
%zoek_max_brug((R,K,eiland(M)),B, Max) :-
%	zoek_buren(R,K,B,Buren),
%	zoek_eiland(R,Buren,Buureilanden),
%	listlength(Buren,N1),
%	listlength(Buureilanden,



zoek_buren(R,K,B,Lijst) :-
	zoek_horiz_buren(R,K,B,Lijst1),
	zoek_vert_buren(R,K,B,Lijst2),
	Lijst = [Lijst1|Lijst2].
		
zoek_horiz_buren(R,K,B,Lijst) :-
	findall((R,K1,I), 
			((K1 is K-1 ; K1 is K+1), memberchk((R,K1,I),B)),
			Lijst).
			
zoek_vert_buren(R,K,B,Lijst) :-
	findall((R1,K,I), 
			((R1 is R-1 ; R1 is R+1), memberchk((R1,K,I),B)),
			Lijst).

	


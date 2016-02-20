safe([]).
safe([Queen|Others]) :-
 safe(Others),
 noattack(Queen,Others,1).
 
noattack(_,[],_).
 
noattack(Y,[Y1|Ylist],Xdist) :-
 Y1-Y=\=Xdist,
 Y-Y1=\=Xdist,
 Dist1 is Xdist + 1,
 noattack(Y,Ylist,Dist1).


	
queens(L) :-
	permutation([1,2,3,4,5], L),
	 safe(L).
	
	




%notsamerow(Queens) :- 
%	sort(Queens,S),
%	length(Queens,L1),
%	length(S,L2),
%	L1 = L2.


%notsamediagonal([_]).
%notsamediagonal(Queens) :-
%	[Qh|Qr] = Queens,
%	findall( X1-Y1,  
%	length(R,0),
%	notsamediagonal(Qr).

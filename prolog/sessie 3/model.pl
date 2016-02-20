edge(a,b).
edge(b,c).
edge(b,d).
edge(c,a).
edge(c,d).
edge(d,e).
edge(e,a).

closeness(Knoop,Waarde) :- 
	other_nodes(Knoop,Knopen),
	findall(L,(
			member(Y,Knopen),
			shortest_path(Knoop,Y,L)),
		KPaden), 
	sumlist(KPaden,SomPaden), 
	length(Knopen,NumPaden), 
	Waarde is SomPaden/NumPaden.

shortest_path(A,B,ShortestPath) :- 
	findall(L,path(A,B,[],L),List),
	sort(List,[ShortestPath|_]).

other_nodes(Knoop,AndereKnopen)
	findall(X,(
			(edge(_,X);edge(X,_)), 
			X\==Knoop),
		Lijst), 
	sort(Lijst,AndereKnopen).

path(A,B,P,L) :- 
	edge(A,B),
	length([B|P],L). 

path(A,B,P,Length) :-
	edge(A,C), 
	\+ member(C,P), 
	path(C,B,[C|P],L).
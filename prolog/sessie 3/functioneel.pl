inc(X,Y) :- Y is X + 1.

map([],_,[]).

map([X|T1],P,[Y|T2]):-
	T=..[P,X,Y],
	call( T ),
	map(T1,P,T2).
  
	
	
	

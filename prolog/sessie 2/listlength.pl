listlength([], N, N). % Second argument is the accumulator. 

listlength([_|T], L, N) :- 
L1 is L + 1, 
listlength(T, L1, N). 

listlength(List, N) :- listlength(List, 0, N). 


laatste_element([L],L).

laatste_element([_|T], L) :- 
laatste_element(T, L). 

next_to([A,B|_],A,B).
next_to([_|T],A,B) :- next_to(T,A,B).
	

%vector_sum([],[],0).
%vector_sum([A],[B],[C]) :- C is A + B.
%vector_sum([A|T1],[B|T2],X) :-  vector_sum(T1,T2,X).

%pair(A,B).
lookup([pair(A,B)|_],A,B).
lookup([pair(_,_)|T],C,D):-lookup(T,C,D).



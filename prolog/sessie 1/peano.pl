peano_plus(nul,X,X).
peano_plus(s(X),Y,s(Z)) :- peano_plus(X,Y,Z).

min(X,X,nul).
min(s(X),Y,s(Z)) :- min(X,Y,Z).

groter_dan(X,nul) :- X \== nul.
groter_dan(Y,s(Z)) :- Y\==s(Z).


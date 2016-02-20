% Dit bestand bevat 4 verschillende oplossingen voor boom_lijst/2.


% Strategie 1: met accumulator
boom_lijst1(Boom,Lijst) :-
	boom_lijst1(Boom,[],Lijst).
	
boom_lijst1(leeg,L,L).
boom_lijst1(knoop(L,W,R),Lin,[[W|H]|Lout]) :-
	splits(Lin,H,T),
	boom_lijst1(R,T,Ltmp),		% Eerste rechts dan links => 
	boom_lijst1(L,Ltmp,Lout).	%       zorgt voor de juiste volgorde
	
% Splits een lijst van lijsten in Head en Tail (werkt ook voor lege lijst).
splits([],[],[]).
splits([H|T],H,T).

% ==========================================================================

% Strategie 2: met append
boom_lijst2(leeg,[]).
boom_lijst2(knoop(L,W,R),[[W]|Lijst]) :-
	boom_lijst2(L,LijstL),
	boom_lijst2(R,LijstR),
	voeg_samen(LijstL,LijstR,Lijst).
	
% Voeg twee lijsten samen door hun overeenkomstige elementen te combineren.
% (zie vector_sum uit de oefenzitting)
voeg_samen(X,[],X). 
voeg_samen([],[A|X],[A|X]). 
voeg_samen([A|R],[B|S],[C|T]) :-
	append(A,B,C),
	voeg_samen(R,S,T).

% ==========================================================================

% Strategie 3: met keysort/2  (Steven Vanden Eynde)
% Volgens de documentatie: 
%    keysort/2 sorteert een lijst [A-X,B-Y,C-Z,...] op basis van de keys (A,B,...)
%		EN bij gelijke keys blijven de waarde in dezelfde volgorde staan.

boom_lijst3(Boom,Lijst) :-
	maak_lijst(Boom,UnsortedLijst,1),
	keysort(UnsortedLijst,KeySortedLijst),
	zet_om(KeySortedLijst,Lijst).
	
% Maak een lijst van Diepte-Waarde paren.
maak_lijst(leeg,[],_).
maak_lijst(knoop(L,W,R),[D-W|Rest],D) :-
	ND is D + 1,
	maak_lijst(L,RestL,ND),
	maak_lijst(R,RestR,ND),
	append(RestL,RestR,Rest).
	
% Zet de gesorteerde lijst van Diepte-Waarde paren in het goede formaat.
zet_om([],[]).
zet_om(KeyLijst,[SubLijst1|SubLijst2]) :-
	zet_om_sub(KeyLijst,SubLijst1,KeyLijst2),
	zet_om(KeyLijst2,SubLijst2).
	
zet_om_sub([_-W],[W],[]).
zet_om_sub([D-W1,D-W2|Rest1],[W1|Rest2],RestLijst) :-
	zet_om_sub([D-W2|Rest1],Rest2,RestLijst).
zet_om_sub([D1-W1,D2-W2|Rest],[W1],[D2-W2|Rest]) :- D1 \== D2.


% ==========================================================================

% Strategie 4: deze oplossing werkt in beide richtingen 
%   (naar een idee van Gert Thijs)

boom_lijst4(Boom,Lijst) :- boom_lijst4([Boom], [], [], Lijst).

boom_lijst4([],[],[],[]) :- !.
boom_lijst4([],[],V,[V]) :- !.
boom_lijst4([],Kinderen,Waarden,[Waarden|Lijst]) :- 
	boom_lijst4(Kinderen,[],[],Lijst). 
boom_lijst4([leeg|T],Kinderen,Waarden,Lijst) :- 
	boom_lijst4(T,Kinderen,Waarden,Lijst).
boom_lijst4([knoop(L,W,R)|T], Kinderen, Waarden, Lijst) :-
	append(Waarden,[W],Waarden2),
	append(Kinderen,[L,R],Kinderen2),
	boom_lijst4(T,Kinderen2,Waarden2,Lijst).

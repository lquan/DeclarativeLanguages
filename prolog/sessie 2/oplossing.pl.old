% Li Quan
% 1e master computerwetenschappen optie artificiele intelligentie

% Voorbeeldbomen
boom(1,knoop(knoop(knoop(leeg,4,leeg),2,leeg),1,knoop(knoop(leeg,5,leeg),3,knoop(leeg,6,leeg)))).
boom(2,knoop(knoop(knoop(knoop(leeg,d,leeg),c,leeg),b,knoop(leeg,e,leeg)),a,knoop(knoop(leeg,g,leeg),f,knoop(leeg,h,leeg)))).
boom(3,knoop(knoop(knoop(leeg,c,leeg),b,knoop(leeg,e,leeg)),a,knoop(knoop(leeg,g,leeg),f,knoop(leeg,h,leeg)))).

%geeft de lijst met elementen terug van de boom op gegeven diepte 
lijst_diepte(leeg,_,[]).
lijst_diepte(knoop(_,W,_),1,[W]).
lijst_diepte(knoop(L,_,R),Diepte,Lijst) :- 
	Diepte > 1, 
	D1 is Diepte-1,
	lijst_diepte(L,D1,L1),
	lijst_diepte(R,D1,L2), 
	append(L1,L2,Lijst).

boom_lijst(leeg,[]).
boom_lijst(Boom,Lijst) :- boom_lijst(Boom,Lijst,1).
boom_lijst(Boom,[],D) :- lijst_diepte(Boom,D,[]), !.
boom_lijst(Boom,Lijst,D) :- 
	lijst_diepte(Boom,D,LD),
	D1 is D+1, 
	boom_lijst(Boom,S1,D1), 
	append([[LD],S1],Lijst).


% efficientere niet werkende breadth first search van de boom
%boom_lijst(leeg,[]).
%boom_lijst(knoop(leeg,W,leeg),[[W]]).

%boom_lijst(knoop(L,W,R),Result) :- boom_lijst(L,X), boom_lijst(R,Y), 
%				X1 = [Xh|Xt], Y1 = [Y|T],
%				append([[W]],[X1,Y1],Result).
				

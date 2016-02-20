% De code in dit bestand is enkel bedoeld als hulpmiddel bij het oproepen van het 
%  zelfgedefinieerde predikaat 'hashi(Bord)'
%
% Het predikaat 'losop(PuzzelId)' maakt eerst een Bord op basis van de 
%  voorgedefinieerde puzzel (in 'puzzels.pl').
% Vervolgens wordt 'hashi(B)' opgeroepen en wordt de uiteindelijke oplossing 
%  getoond.
%
% losop_gui(PuzzelId) doet hetzelfde maar gebruikt een grafische interface.
%
% Het is NIET de bedoeling dat jullie deze predikaten oproepen vanuit jullie code.
%
% Dit bestand kan automatisch ingeladen worden door de regel 
%	:- ensure_loaded(opgave).
% in jullie oplossing bestand te zetten.

:- ensure_loaded(puzzels).
	
losop(PuzID) :-
	maak_opgave(PuzID,B,M,N),
	write('Puzzel '), write(PuzID), nl,
	hashi(B),
	toon_oplossing(M,N,B).

losop_time(PuzID) :-
	maak_opgave(PuzID,B,M,N),
	write('Puzzel '), write(PuzID), nl,
	time(hashi(B)),
	!,
	toon_oplossing(M,N,B).

losop_gui(PuzId) :-
	ensure_loaded(gui),
	maak_opgave(PuzId,B,M,N),
	init_gui(N,M),
	update_gui(B),
	hashi(B),
	update_gui(B).

toon_puzzel(PuzId) :-
	ensure_loaded(gui),
	maak_opgave(PuzId,Bord,M,N),
	init_gui(N,M),
	update_gui(Bord).

maak_opgave(PuzId,Bord,M,N) :-
	puzzel(PuzId,PuzList),
	length(PuzList,M),
	PuzList = [P|_],
	length(P,N),
	findall((I,J,Y),(between(1,M,I),between(1,N,J),nth1(I,PuzList,Rij),nth1(J,Rij,X), (nonvar(X) -> Y = eiland(X); true  )), Bord).
		
toon_oplossing(M,N,Bord) :-
	forall(between(1,M,I), (forall(between(1,N,J), (
		memberchk((I,J,Val),Bord),
		(var(Val) ->
			write(' ')
		; (Val = eiland(X) ->
			write(X)
		; (Val = brug(_,1) ->
			write(+)
		; (Val = brug(_,2) ->
			write(#)
		)))))),nl)).

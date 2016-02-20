% hashiwokakero

:- ensure_loaded(opgave).

% Voorstelling:
%	Puzzel -> (RijIndex,KolomIndex,Constraint) (enkel eilanden)
%	Bord -> (RijIndex,KolomIndex,Inhoud)
%			Inhoud -> 	brug(Richting,Breedte)
%						eiland(Constraint)
%						Variable (nog niet ingevuld of lege cel)
%
% Strategie:
% 	Handel alle eilanden af in volgorde van linksboven naar rechtsonder
%		-> geen symmetrieproblemen
%		-> volstaat om bruggen te bouwen naar omlaag of naar rechts
%
% Symmetriebreking:
%	Brug mag enkel naar onder als er nog geen brug naar rechts gebouwd werd.
%	(Dus eerst onder, dan rechts, en nooit omgekeerd.)
%
% Deze oplossing probeert zo vroeg mogelijk op verbondenheid te testen.
% Zie andere modeloplossing voor een late test (die zou hier ook gebruikt kunnen worden).

hashi(Bord) :-
	eilanden(Bord,Puzzel),
	stap(Puzzel,_,Bord,[]).



eilanden([],[]).
eilanden([(_,_,X)|T],S) :-
	var(X), 
	!,
	eilanden(T,S).
eilanden([(I,J,eiland(Q))|T],[(I,J,Q)|S]) :-
	eilanden(T,S).

% Alle eilanden zijn volledig verwerkt: oplossing gevonden.
stap([],_,_,[_]).	% Laatste argument test verbondenheid => er mag maar 1 component zijn (zie verder).

% Eerste eiland is volledig verwerkt: ga verder met de rest.
% (Vergeet de richting-constraint.)
stap([(_,_,0)|Puzzel],_,Bord,Verbonden) :-
	!,
	stap(Puzzel,_,Bord,Verbonden).
	
% Eerste eiland is nog niet volledig verwerkt: verwerk dit eerst.
% Bij de eerste oproep voor een eiland is Richting niet gebonden.
% Bij een tweede oproep voor hetzelfde eiland is de Richting 'v'.
% Dit zorgt ervoor dat er alleen een horizontale brug gebouwd kan worden als er nog geen verticale gebouwd is.
stap([(I,J,Getal)|Puzzel],Richting,Bord,Verbonden) :-

	% Bepaal breedte van brug: als Getal = 1 dan kan alleen enkele brug.
	% Als Getal > 1, laat eindpunt breedte bepalen (laat Breedte variabele). 
	(Getal = 1 -> Breedte = 1; true),

	% Zet een brug, Richting en Breedte kunnen variabel zijn.
	brug(Richting,Breedte,(I,J,Getal),Puzzel,Bord,(Ie,Je,NewN)),
	
	% Update constraints voor eindpunt in puzzel en bord.
	vervang_vakje(Puzzel,(Ie,Je,_),(Ie,Je,NewN),NPuzzel),
	vervang_vakje(Bord,(Ie,Je,_),(Ie,Je,eiland(NewN)),NBord),
		
	NGetal is Getal - Breedte,
	
	update_verbonden(Verbonden,(I,J,eiland(NGetal)),(Ie,Je,eiland(NewN)),Breedte,Verbonden1),

	vervang_vakje(NBord,(I,J,_),(I,J,eiland(NGetal)),NBord1),
	% Recursieve oproep: pas constraint op huidige eiland aan,
	%  en laat enkel een horizontale brug toe (symmetrie-breking).
	stap([(I,J,NGetal)|NPuzzel],v,NBord1,Verbonden1).	
	
% Zet een brug:
%  Richting:  horizontal of vertical (mag variabele zijn)
%  Breedte: 1 of 2 (mag variabele zijn)
%  Huidige locatie
%  Puzzel: nog te verwerken eilanden
%  Bord: bord met bijgewerkte constraints
%  Eind: eindpunt van de brug
brug(Richting,Breedte,(I,J,_),Puzzel,Bord,Eind) :-
	% Verplaats 1 vakje in Richting (als Richting variabele, dan zijn er twee opties).
	beweeg(Richting,I,J,I1,J1),
	
	% Test of het nieuwe vakje bestaat (binnen bord ligt), en vraag de waarde op.
	memberchk((I1,J1,V1),Bord),

	(var(V1) ->
		% Vakje is nog leeg: zet brugdeel (Breedte kan nog steeds variabel zijn).
		V1 = brug(Richting,Breedte),
		% Ga verder (Richting ligt nu vast, Breedte kan nog steeds variabel zijn).
		brug(Richting,Breedte,(I1,J1,V1),Puzzel,Bord,Eind)
	; (V1 = eiland(N) -> 
		% Vakje is een eiland met constraint N.
		% Leg Breedte vast. Probeer eerst 2 (als N>1), en daarna 1 (als N>0). 
		((N > 1, Breedte = 2) ; (N > 0, Breedte = 1)),
		% Stel eindpunt van de brug in.
		NewN is N - Breedte,
		Eind = (I1,J1,NewN)		
	;
		% Vakje bevat al een brugdeel: faal.
		fail	
	)).
	
% Beweeg een vakje omlaag of naar rechts.
% Deze twee richtingen zijn voldoende omdat de puzzel van linksboven naar rechtsonder 
%   wordt opgelost.
beweeg(h,I,J,I,J1) :- J1 is J + 1.
beweeg(v,I,J,I1,J) :- I1 is I + 1.

% Vervang een vakje in Puzzel of Bord.
vervang_vakje([Tuple|Rest],Tuple,NewTuple,[NewTuple|Rest]) :- !.
vervang_vakje([X|Rest],Tuple,NewTuple,[X|NewRest]) :- 
	vervang_vakje(Rest,Tuple,NewTuple,NewRest).


% ====================================================================================
% 									VERBONDENHEIDSTEST
% ====================================================================================

% Deze test houdt de 'componenten' van het netwerk bij.
% Dit zijn alle groepen van onderling verbonden eilanden.
% Per component wordt ook de nog overgebleven quota bijgehouden, dat wil zeggen,
%  het totale aantal bruggen dat nog kan vertrekken uit een eiland binnen deze
%  component. Als dit quota 0 wordt, dan is het dus onmogelijk om de component
%  nog te verbinden met andere eilanden. Dit mag dus enkel gebeuren als er maar 
%  1 component over is.
% Een component wordt voorgesteld als 
%	component(OvergeblevenCapaciteit, LijstVanEilandCoordinaten)

update_verbonden(DataIn,(I1,J1,C1),(I2,J2,C2),Breedte,DataOut) :-
	update_componenten(DataIn,(I1,J1,C1),(I2,J2,C2),Breedte,DataOut).
	
update_componenten(Componenten,(I1,J1,C1),(I2,J2,C2),Breedte,
                                                 [component(CapNew,LijstNew)|RestComp1]) :-
	select_component(Componenten,(I1,J1,C1),Breedte,component(CapC1,LijstC1),RestComp),
	(member((I2,J2),LijstC1) ->		% Als eiland 2 in zelfde component zit:
		CapNew is CapC1 - Breedte,		% pas capaciteit nog eens aan
		LijstNew = LijstC1,				% gebruik andere gegevens van dezelfde component
		RestComp1 = RestComp
	;
		select_component(RestComp,(I2,J2,C2),Breedte,Comp2,RestComp1),	% Zoek de component met eiland 2
		merge_componenten(component(CapC1,LijstC1),Comp2,component(CapNew,LijstNew)) % Voeg componenten samen
	),
	(CapNew = 0 -> RestComp1 = []; true).
	
% Voeg twee componenten samen (omdat ze met elkaar verbonden worden met een nieuwe brug).		
merge_componenten(component(C1,L1),component(C2,L2),component(C,L)) :-
	append(L1,L2,L),
	C is C1 + C2.
		
% Vind de component voor het gegeven eiland in de lijst van componenten.
% select_component(InvoerLijst,TeVindenEiland,BreedteVanBrug,GezochteComponent,RestVanLijst)
select_component([component(Capaciteit,Lijst)|T],(I,J,_Cap),Breedte,
                                                  component(Capaciteit2,Lijst),T) :-
	member((I,J),Lijst),	% Eiland zit in deze component.
	!,						% Stop met verder zoeken.
	Capaciteit2 is Capaciteit - Breedte.	% Update capaciteit (- breedte van de brug)
select_component([C|T],Cel,Breedte,Comp,[C|S]) :-
	select_component(T,Cel,Breedte,Comp,S).

% Einde van de lijst bereikt en eiland niet gevonden -> maak een nieuwe component.
select_component([],(I,J,eiland(Cap)),_,component(Cap,[(I,J)]),[]).

:- ensure_loaded(opgave).

% Alternatieve (snelle) oplossing.
% 
% Puzzels 0-11: (inferences)
%	bridges_new 309 1190 22755 98503 12287 15664 8367 6578 9050 4252 9585  | 188540
% Puzzel 12: 10,150,673 =>  3.3sec
% Puzzel 13: 48,865,809 => 15.7sec
%
% Deze oplossing transformeert eerst het bord naar een lijst van eilanden waarin elk eiland wordt voorgesteld als:
% 	eiland(I,J,Quota,Noord,Oost,Zuid,West,Var)
%		I: rijnummer
%		J: kolomnummer
%		Quota: originele quota
%		Noord: lijst met vakjes tot aan het buureiland in noordelijke richting ([] als er geen buur is)
%			Deze lijst eindigt altijd met een vakje eiland((I1,J1)), met (I1,J1) de coordinaat van het buureiland.
%		Oost / Zuid / West: zie Noord
%		Var: ongebonden variabele, wordt gebruikt in de verbondenheidstest
% 
% Bijvoorbeeld in het bord:
%	A 2 B C 3
%	  D
%	  E
%	  F
%	  4
% wordt het eerste eiland voorgesteld als eiland(1,2,2,[],[],[B,C,eiland((1,5))],[D,E,F,eiland((5,2))],_).
%
% Het voordeel van deze voorstelling is dat er in constante tijd toegang is tot de buurvakjes 
%  (er moet dus niet opgezocht worden in het hele Bord op basis van coördinaat).
%
% Quota worden niet bijgewerkt maar telkens berekend, dit kan in constante tijd dankzij de voorstelling.
%
% Er wordt 1 heuristiek gebruikt: 1---1 en 2===2 mogen niet.
% Opmerking: omwille van deze heuristiek werkt deze oplossing NIET voor een bord met maar twee eilanden, 
%  maar dit kan met een eenvoudige test opgelost worden.
%
% Opmerking: enkel het predikaat zet_bruggen/5 definieert backtrackpunten.
% 	hashi/1, stap/1 en zet_bruggen/5 								=> nondet [0..]
% 	vind_buur_hulp/4 en cel/3 en test_verbonden/1 en zet_brug/4		=> semidet [0..1]
%	tel_brug/2 en tel_quota/3 en beweeg/3							=> det	[1]

hashi(Bord) :-
	eilanden(Bord,Bord,Eilanden),	% transformeer het bord naar de alternatieve voorstelling
	stap(Eilanden),					% stap door de eilanden en los ze 1 voor 1 op
	test_verbonden(Eilanden).		% test of alle eilanden verbonden zijn

% Stap door de eilanden en los ze een voor een op.	
stap([]).
stap([(_,_,Quota,N,W,Z,O,Var)|Eilanden]) :-
	tel_quota([(v,N),(h,W),(v,Z),(h,O)],0,AlVoldaan),
	TeDoen is Quota - AlVoldaan,
	zet_bruggen(TeDoen,(Quota,Var),Z,O,Eilanden),	% Quota en Var worden doorgegeven voor heuristiek en verbondenheid.
	stap(Eilanden).

% Zet bordvoorstelling om naar eilandenvoorstelling. 
% Zorg ervoor dat de variabelen behouden blijven (niet met findall dus).
eilanden([],_,[]).
eilanden([(_,_,C)|Rest],Bord,Eilanden) :-
	var(C), !, eilanden(Rest,Bord,Eilanden).	% vakje bevat geen eiland
eilanden([(I,J,eiland(Q))|Rest],Bord,[(I,J,Q,N,W,Z,O,_)|RestU]) :-
	vind_buur((-1,0),(I,J),Bord,N),	% boven: rij-1
	vind_buur((0,-1),(I,J),Bord,W),	% links: kolom-1
	vind_buur((1,0),(I,J),Bord,Z),	% onder: rij+1
	vind_buur((0,1),(I,J),Bord,O),	% rechts: kolom+1
	eilanden(Rest,Bord,RestU).

%% =============================================================================
%%	Hulppredikaten voor eilanden/3
%% =============================================================================
% Vind de lijst van vakjes tot aan het buureiland.
% Dit faalt nooit, maar als er geen buureiland is, dan wordt [] teruggegeven.
% Wrapper rond vind_buur_hulp
vind_buur(D,X,Bord,Buren) :-
	(vind_buur_hulp(D,X,Bord,Buren) ->
		true
	;
		Buren = []
	).

% Vind de lijst van vakjes tot aan het buureiland. Als er geen buureiland is dan faalt dit.
% Werkt enkel op het initiele bord (met enkel Var en eiland(Q) dus).
% Lijst bevat enkel de inhoud van de vakjes, zonder coordinaten dus.
% Het laatste vakje in de lijst is van de vorm eiland((I,J)).
vind_buur_hulp(Delta,Cel,Bord,Buren) :-
	beweeg(Delta,Cel,VolgendeCel),
	cel(VolgendeCel,Bord,Waarde),
	( var(Waarde) ->
		Buren = [Waarde|Buren1],
		vind_buur_hulp(Delta,VolgendeCel,Bord,Buren1)
	; Waarde = eiland(_) ->
		Buren = [eiland(VolgendeCel)]
	).

% Pas de coordinaten van de cel aan. Dankzij dit hulppredikaat moeten we vind_buren(_hulp) maar één keer definiëren.
beweeg((DI,DJ),(I,J),(I1,J1)) :-
	I1 is I + DI,
	J1 is J + DJ.
	
% Vraag een cel op uit het bord.
cel((I,J),Bord,W) :-
	memberchk((I,J,W),Bord).
%% =============================================================================

% Tel alle buurbruggen.
tel_quota([],Acc,Acc).
tel_quota([H|T],Acc,Res) :-
	tel_brug(H,Aantal),
	Acc1 is Acc + Aantal,
	tel_quota(T,Acc1,Res).

% Tel waarde van de brug aan het begin van de lijst.
% Enkel bruggen in de correcte richting worden geteld 
%  (een brug in een horizontaal buurvakje moet ook horizontaal zijn, idem voor verticaal)
tel_brug((_,[Waarde|_]),0) :- var(Waarde), !.						% geen brug
tel_brug((Richting,[brug(Richting,Breedte)|_]),Breedte) :- !.		% brug in juiste richting
tel_brug(_,0).														% geen buur of brug foute richting
	
% Zet exact het opgegeven aantal bruggen. 
% Expliciete definities voor de 5 gevallen voor het aantal bruggen (0,1,2,3,4) (meer bruggen plaatsen kan niet).
% Bruggen kunnen enkel richting Zuid en Oost gezet worden.
% Volgorde van de individuele gevallen kan een grote impact hebben op de resultaten voor individuele puzzels.
zet_bruggen(0,_,_,_,_).

% 1 brug: 1h of 1v
zet_bruggen(1,Start,_,O,Eilanden) :- zet_brug(O,Start,Eilanden,brug(h,1)).
zet_bruggen(1,Start,Z,_,Eilanden) :- zet_brug(Z,Start,Eilanden,brug(v,1)).

% 2 bruggen: 1h+1v of 2h of 2v
zet_bruggen(2,Start,Z,O,Eilanden) :-
	zet_brug(Z,Start,Eilanden,brug(v,1)),zet_brug(O,Start,Eilanden,brug(h,1)).
zet_bruggen(2,Start,Z,_,Eilanden) :- zet_brug(Z,Start,Eilanden,brug(v,2)).
zet_bruggen(2,Start,_,O,Eilanden) :- zet_brug(O,Start,Eilanden,brug(h,2)).

% 3 bruggen: 1h+2v of 2h+1v
zet_bruggen(3,Start,Z,O,Eilanden) :-
	zet_brug(Z,Start,Eilanden,brug(v,2)),zet_brug(O,Start,Eilanden,brug(h,1)).
zet_bruggen(3,Start,Z,O,Eilanden) :-
	zet_brug(Z,Start,Eilanden,brug(v,1)),zet_brug(O,Start,Eilanden,brug(h,2)).

% 4 bruggen: 2h+2v
zet_bruggen(4,Start,Z,O,Eilanden) :-
	zet_brug(Z,Start,Eilanden,brug(v,2)),zet_brug(O,Start,Eilanden,brug(h,2)).

% Zet brug. Aan het einde bevat het veld een 'eiland((I,J))' vakje.
% De tweede parameter (Q1,Var) zorgt voor twee optimalisaties.
%	Q1 is het quota van het vertrekeiland van de brug, Var is de ongebonden variable voor het eiland.
%	1) Er is geen brug toegelaten van breedte X tussen twee eilanden met quota X (bvb. 1-1 en 2=2).
%	2) Var stelt de variabelen van de verbonden eilanden gelijk. Hierdoor wordt de verbondenheid test 
%		supereenvoudig.
zet_brug([eiland((I,J))],(QuotaBron,VarBron),Eilanden,brug(_,B)) :-
	!,
	memberchk((I,J,Quota,N,W,Z,O,Var),Eilanden),
	Var = VarBron,	% voor verbondenheidstest (expliciete toekenning voor de duidelijkheid)
	\+ (QuotaBron == Quota, QuotaBron == B), % heuristiek
	tel_quota([(v,N),(h,W),(v,Z),(h,O)],0,AlVoldaan),
	Quota >= AlVoldaan.

% 'Term' bevat de beschrijving van de brug. Dit lukt zolang er geen andere brug in de weg staat.
zet_brug([Term|Lijst],Start,Eilanden,Term) :-
	zet_brug(Lijst,Start,Eilanden,Term).
	
% Test verbondenheid. Als alle eilanden aan elkaar hangen dan zijn de ongebonden variabelen allemaal
%  aan elkaar gelijkgesteld (zie 'Var = VarBron' in zet_brug). Een waarde toekennen aan de eerste 
% (bvb. 'a') moet er dus voor zorgen dat ze allemaal gelijk zijn aan 'a', en dat dus geen 'b' kan 
% gevonden worden.
test_verbonden([(_,_,_,_,_,_,_,a)|Rest]) :- 
	\+ memberchk((_,_,_,_,_,_,_,b),Rest).
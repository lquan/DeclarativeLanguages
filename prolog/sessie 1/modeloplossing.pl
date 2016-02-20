% Modeloplossing: Prolog 1

vriend_van(annelies,bart).
vriend_van(annelies,chris).
vriend_van(chris,daniel).
vriend_van(bart,annelies).
vriend_van(chris,annelies).
vriend_van(daniel,chris).

fan(annelies,muziek).
fan(annelies,feestjes).
fan(bart,muziek).
fan(bart,feestjes).
fan(chris,feestjes).
fan(daniel,muziek).
fan(daniel,voetbal).

veel_interesses(P) :- 
	fan(P,O1),
	fan(P,O2),
	O1 \== O2.

beveel_aan(P,O) :-	
	vriend_van(P,V1),
	vriend_van(P,V2),
	V1 \== V2,
	fan(V1,O),
	fan(V2,O).

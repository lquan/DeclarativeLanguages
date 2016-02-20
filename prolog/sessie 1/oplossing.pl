% Li Quan
% 1e master computerwetenschappen

bevriend(annelies,bart).
bevriend(annelies,chris).
bevriend(chris,daniel).
bevriend(bart,annelies).
bevriend(chris,annelies).
bevriend(daniel,chris).

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
                   bevriend(P,V1),
	 	   bevriend(P,V2),
		   V1 \== V2,
		   fan(V1,O),
		   fan(V2,O).


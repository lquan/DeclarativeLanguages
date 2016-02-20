% beweeg/3: beweegt een tape in een bepaalde richting.
beweeg(links,tape([],Teken,Rs),tape([],#,[Teken|Rs])).
beweeg(links,tape([L|Ls],Teken,Rs),tape(Ls,L,[Teken|Rs])).
beweeg(rechts,tape(Ls,Teken,[]),tape([Teken|Ls],#,[])).
beweeg(rechts,tape(Ls,Teken,[R|Rs]),tape([Teken|Ls],R,Rs)).
beweeg(wacht,Tape,Tape).
% lees/2: Leest het huidige teken van de band
lees(tape(_,Teken,_),Teken).
% schrijf/3: Schrijft een teken op de band
schrijf(Teken,tape(L,_,R),tape(L,Teken,R)).

%regel(HuidigToestand,HuidigTeken,VolgToestand,NieuwTeken,Beweging) :-
regel(qs,1,q0,#,rechts).
regel(q0,1,q1,#,rechts).
regel(q0,#,q2,0,wacht).
regel(q1,1,q0,#,rechts).
regel(q1,#,q2,1,wacht).	
start(qs).
stop(q2).

tape(Start, Stop, L) :- 
	

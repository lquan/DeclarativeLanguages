:- pce_dispatch([]).

init_gui(W,H) :-
	(object(@bv) ->
		send(@bv,slot,grid_width,W),
		send(@bv,slot,grid_height,H)
	;
		new(@bv,board_vis(W,H))
	),
	get(@bv,frame,DP),
	new(Label,char_array('Bridges')),
	send(DP,label,Label),
	send(@bv,height,H*20),
	send(@bv,width,W*20),
	send(@bv,open),
	nb_setval(cnt,1).
		
update_gui(Board) :-
	(object(@bv) ->
		findall(B2,(member((_,_,B),Board),transform_data(B,B2)),Board2),
		chain_list(CL,Board2),
		send(@bv,grid,CL)
	;
		true
	).
	
close_gui :-
	free(@bv).

transform_data(X,-100) :- var(X), !.
transform_data(eiland(N),N).
transform_data(brug(h,D),E) :- E is -D.
transform_data(brug(v,D),E) :- E is -D-2.

:- use_module(library(pce)).	
:- pce_begin_class(board_vis, picture, "Board visualizer component").

variable(grid_height, '1..|size*', get, "Height of board").
variable(grid_width, '1..|size*', get, "Width of board").
variable(grid,chain, get, "Contents of board").

initialise(P,GridWidth:['1..|size*'],GridHeight:['1..|size*']) :->
	send(P,send_super,initialise),
	send(P,slot,grid_width,GridWidth),
	send(P,slot,grid_height,GridHeight).
	
'_redraw_area'(P, A:area) :->
	get(P,grid_height,GH),
	get(P,grid_width,GW),
	get(P,grid,GC),
	chain_list(GC,G),
	object(A, area(X, Y, W, H)),
	BW is W/GW,
	BH is H/GH,
	M is (GW*GH)-1,
	send(P,save_graphics_state),
	send(P,graphics_state,1,none,black),
	forall(between(0,M,I),
		(
			X1 is X+(I mod GW)*BW,
			Y1 is Y+(I // GW)*BH,
			nth0(I,G,V),
			(V == -100 ->
				true
			;
				(V >= 0 ->
					send(P,draw_arc,X1,Y1,BW,BH),
					send(P,draw_text,V,@courier_roman_12,X1,Y1,BW,BH,center,center)

				; 
					(V == -1 ->
						Y2 is Y1 + BH/2,
						X2 is X1 + BW,
						send(P,draw_line,X1,Y2,X2,Y2)
					; (V == -2 ->
						Y2 is Y1 + 2*BH/5,
						Y3 is Y1 + 3*BH/5,
						X2 is X1 + BW,
						send(P,draw_line,X1,Y2,X2,Y2),
						send(P,draw_line,X1,Y3,X2,Y3)
					; (V == -3 ->
						X2 is X1 + BW/2,
						Y2 is Y1 + BH,
						send(P,draw_line,X2,Y1,X2,Y2)
					; 
						X2 is X1 + 2*BW/5,
						X3 is X1 + 3*BW/5,
						Y2 is Y1 + BH,
						send(P,draw_line,X2,Y1,X2,Y2),
						send(P,draw_line,X3,Y1,X3,Y2)
						
					))) 
				)
			)
			
		)
	),		
	send(P,restore_graphics_state).
	
grid(P,Grid:chain) :->
	send(P,slot,grid,Grid),
	send(P,redraw).
	
:- pce_end_class.
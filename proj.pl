


aplica_R1_triplo(Lista, R) :- Lista == [_,_,_], !, R = Lista.
aplica_R1_triplo([1,1,X], R) :- var(X), !, R = [1, 1, 0].
aplica_R1_triplo([0,0,X], R) :- var(X), !, R = [0, 0, 1].
aplica_R1_triplo([1,X,1], R) :- var(X), !, R = [1, 0, 1].
aplica_R1_triplo([0,X,0], R) :- var(X), !, R = [0, 1, 0].
aplica_R1_triplo([X,1,1], R) :- var(X), !, R = [0, 1, 1].
aplica_R1_triplo([X,0,0], R) :- var(X), !, R = [1, 0, 0].
aplica_R1_triplo([1,X,0], R) :- var(X), !, R = [1, _, 0].
aplica_R1_triplo([0,X,1], R) :- var(X), !, R = [0, _, 1].
aplica_R1_triplo(Lista, R) :- Lista \= [1,1,1], Lista \= [0,0,0], !, R = Lista.



/*aplica_R1_fila_aux(Fila, N_Fila) :- AUX = [X, Y, Z | Resto],
    aplica_R1_triplo([X,Y,Z], N_Fila), aplica_R1_fila_aux(Resto, AUX).*/



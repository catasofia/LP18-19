:-[codigo_comum].


%/--------------------------------\
%|      FUNCOES AUXILIARES        |
%\--------------------------------/

%conta_elementos_1(Lista, N): N e o numero de 1s existentes na Lista
conta_elementos_1(Lista, N) :- 
    findall(X, (member(X,Lista), X == 1), Aux),
    length(Aux, N).



%conta_elementos_0(Lista, N): N e o numero de 0s existentes na Lista
conta_elementos_0(Lista, N) :-
    findall(X, (member(X,Lista), X == 0), Aux),
    length(Aux, N).



%trocar(Lista, Elem) :- funcao que unifica na Lista recebida todas as variaveis com Elem.
trocar([],_).
trocar([H | T], Elem) :- 
    var(H),
    H = Elem,
    trocar(T, Elem).
trocar([H | T], Elem) :- 
    number(H),
    trocar(T, Elem).



%compara_Listas(Lista, Puzzle): funcao que retorna True se a Lista nao estiver contida
%no Puzzle e False em caso contrario.
compara_listas(_,[]).
compara_listas(Fila, [Linha | RLista]) :-
    Fila \== Linha, !, 
    compara_listas(Fila, RLista).
compara_listas(Fila, [Linha|_]) :-
    Fila == Linha, !, fail.




aplica_R1_triplo(Lista, R) :- Lista = [Y,X,Y], var(Y), number(X), !, R = Lista.
aplica_R1_triplo(Lista, R) :- Lista = [Y,Y,X], var(Y), number(X), !, R = Lista.
aplica_R1_triplo(Lista, R) :- Lista = [X,Y,Y], var(Y), number(X), !, R = Lista.
aplica_R1_triplo(Lista, R) :- Lista = [Y,Y,Y], var(Y), !, R = Lista.
aplica_R1_triplo([1,1,X], R) :- var(X), !, R = [1, 1, 0].
aplica_R1_triplo([0,0,X], R) :- var(X), !, R = [0, 0, 1].
aplica_R1_triplo([1,X,1], R) :- var(X), !, R = [1, 0, 1].
aplica_R1_triplo([0,X,0], R) :- var(X), !, R = [0, 1, 0].
aplica_R1_triplo([X,1,1], R) :- var(X), !, R = [0, 1, 1].
aplica_R1_triplo([X,0,0], R) :- var(X), !, R = [1, 0, 0].
aplica_R1_triplo([1,X,0], R) :- var(X), !, R = [1, X, 0].
aplica_R1_triplo([0,X,1], R) :- var(X), !, R = [0, X, 1].
aplica_R1_triplo(Lista, R) :- Lista \= [1,1,1], Lista \= [0,0,0], !, R = Lista.



aplica_R1_fila_aux(Fila, N_Fila) :- 
    length(Fila, X), 
    X == 3, !, 
    aplica_R1_triplo(Fila, N_Fila).

aplica_R1_fila_aux(Fila, N_Fila) :- 
    Fila = [X, Y, Z | [H | Resto]], !,
    aplica_R1_triplo([X, Y, Z], [X1,Y1,Z1]),
    aplica_R1_fila_aux([Y1, Z1, H|Resto], N_aux), 
    N_Fila= [X1|N_aux].



aplica_R1_fila(Fila, N_Fila) :- 
    aplica_R1_fila_aux(Fila, Aux1),
    aplica_R1_fila_aux(Aux1, Aux2),
    Aux2 \== Aux1,
    aplica_R1_fila(Aux2, N_Fila), !.

aplica_R1_fila(Fila, N_Fila) :- 
    aplica_R1_fila_aux(Fila, Aux1),
    aplica_R1_fila_aux(Aux1, N_Fila), 
    N_Fila == Aux1, !.



aplica_R2_fila(Fila, N_Fila) :- 
    length(Fila, NumElem),
    conta_elementos_1(Fila, Num1),
    Num1 < NumElem // 2,
    conta_elementos_0(Fila, Num2),
    Num2 < NumElem // 2, !,
    N_Fila = Fila.

aplica_R2_fila(Fila, N_Fila) :- 
    length(Fila, NumElem),
    duplicate_term(Fila, N_Fila),
    conta_elementos_1(N_Fila, Num1),
    Num1 =:= (NumElem // 2),
    trocar(N_Fila, 0), !.

aplica_R2_fila(Fila, N_Fila) :- 
    length(Fila, NumElem),
    duplicate_term(Fila, N_Fila),
    conta_elementos_0(N_Fila, Num1),
    Num1 =:= (NumElem // 2),
    trocar(N_Fila, 1), !.



aplica_R1_R2_fila(Fila, N_Fila) :-
    aplica_R1_fila(Fila, Aux1),
    aplica_R2_fila(Aux1, N_Fila), !.



aplica_R1_R2_puzzle_aux([], []).
aplica_R1_R2_puzzle_aux([Fila1 | Resto], [N_Fila1 | Resto1]) :-
    aplica_R1_R2_fila(Fila1, N_Fila1), !,
    aplica_R1_R2_puzzle_aux(Resto, Resto1), !.



aplica_R1_R2_puzzle([],[]).
aplica_R1_R2_puzzle(Puz, N_Puz) :-
    aplica_R1_R2_puzzle_aux(Puz, Aux1),
    mat_transposta(Aux1, Aux2),
    aplica_R1_R2_puzzle_aux(Aux2, Aux3),
    mat_transposta(Aux3, N_Puz), !.
    
    

inicializa(Puz, N_Puz) :-
    aplica_R1_R2_puzzle(Puz, Aux1),
    aplica_R1_R2_puzzle(Aux1, Aux2),
    Aux1 \== Aux2,
    inicializa(Aux2, N_Puz), !.

inicializa(Puz, N_Puz) :-
    aplica_R1_R2_puzzle(Puz, Aux1),
    aplica_R1_R2_puzzle(Aux1, N_Puz),
    N_Puz == Aux1, !.


%verifica_R3(Puzzle): funcao que retorna True se nao existirem linhas ou colunas repetidas e
% false em caso contrario.
verifica_R3([]).
verifica_R3([H1 | Puz]) :-
    compara_listas(H1, Puz), !,
    verifica_R3(Puz).
    


compara_elementos_linhas([],[],_,_,Posicoes, Posicoes).

compara_elementos_linhas(Lista1, Lista2,_ ,_, Posicoes, Posicoes) :-
    Lista1 =@= Lista2, !.

compara_elementos_linhas([H1 | Resto1], [H2 | Resto2], L, C, Posicoes, NPosicoes) :-
    H1 \=@= H2,
    append(Posicoes, [(L, C)], NPosicoes_aux),
    C1 is C + 1, !,
    compara_elementos_linhas(Resto1, Resto2, L, C1, NPosicoes_aux, NPosicoes).

compara_elementos_linhas([H1 | Resto1], [H2 | Resto2], L, C, Posicoes, NPosicoes) :-
    H1 =@= H2,
    C1 is C + 1, !,
    compara_elementos_linhas(Resto1, Resto2, L, C1, Posicoes, NPosicoes).



compara_elementos_colunas([],[],_,_,Posicoes, Posicoes).

compara_elementos_colunas([H1 | Resto1], [H2 | Resto2], L, C, Posicoes, NPosicoes) :-
    H1 \=@= H2,
    append(Posicoes, [(C,L)], NPosicoes_aux),
    C1 is C + 1, !,
    compara_elementos_colunas(Resto1, Resto2, L, C1, NPosicoes_aux, NPosicoes).

compara_elementos_colunas([H1 | Resto1], [H2 | Resto2], L, C, Posicoes, NPosicoes) :-
    H1 =@= H2,
    C1 is C + 1, !,
    compara_elementos_colunas(Resto1, Resto2, L, C1, Posicoes, NPosicoes).



propaga_posicoes([],Puz,Puz).
propaga_posicoes([(Linha, Coluna) | RestoPosicoes], Puz, N_Puz) :-
    duplicate_term(Puz, Aux),
    nth1(Linha, Aux, ListaLinha),
    aplica_R1_R2_fila(ListaLinha, NovaLista),
    mat_muda_linha(Aux, Linha, NovaLista, PuzAux),
    verifica_R3(PuzAux),
    compara_elementos_linhas(ListaLinha, NovaLista, Linha, 1, RestoPosicoes, PosicoesAux),
    mat_transposta(PuzAux, PuzTransp),
    nth1(Coluna, PuzTransp, ListaColuna),
    aplica_R1_R2_fila(ListaColuna, NovaColuna),
    mat_muda_linha(PuzTransp, Coluna, NovaColuna, PuzTranspNovo),
    verifica_R3(PuzTranspNovo),
    compara_elementos_colunas(ListaColuna, NovaColuna, Coluna, 1, PosicoesAux, NPosicoes),
    mat_transposta(PuzTranspNovo, NPuz),
    propaga_posicoes(NPosicoes, NPuz, N_Puz), !.



/* encontra_var_linhas([El | RestoLinha], L, C, Posicoes) :-
    var(El),



resolve(Puz, Sol) :-
    duplicate_term(Puz, PuzAux),
    inicializa(PuzAux, PuzIniciado),
 */
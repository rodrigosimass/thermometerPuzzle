%Rodrigo Simas 81536

:- [exemplos_puzzles]. %load do exemplos_puzzles

% --------------------------------------------------------------------------------------------------
% PREDICADO PROPAGA
% --------------------------------------------------------------------------------------------------
propaga([ListaTerm, _, _], Pos, Posicoes):-
	calculaPosicoes(ListaTerm, Pos, PosicoesAux),
	sort(PosicoesAux, Posicoes).

% --------------------------------------------------------------------------------------------------
% predicado auxiliar calcula_posicoes
% --------------------------------------------------------------------------------------------------
% calcula_posicoes(L,X,P) para uma lista de termometros L procura o termometro que contenho o 
% elemento X e guarda em P todos os elementos entre a base do termometro e X.
% --------------------------------------------------------------------------------------------------
calculaPosicoes(ListaTerm, Pos, PosicoesAux):-
	calculaPosicoes(ListaTerm, Pos, PosicoesAux, []).

calculaPosicoes([[Pos|_]|_], Pos, [Pos|Acc], Acc):-!.

calculaPosicoes([[H|T]|_], Pos, PosicoesAux, Acc):-
	member(Pos, [H|T]), !,
	Acc1 = [H|Acc],
	calculaPosicoes([T|_], Pos, PosicoesAux, Acc1).

calculaPosicoes([[H|T]|OutrosTerm], Pos, PosicoesAux, _):-
	\+(member(Pos, [H|T])),
	calculaPosicoes(OutrosTerm, Pos, PosicoesAux, []). 

% --------------------------------------------------------------------------------------------------
% PREDICADO NAO_ALTERA_LINHAS_ANTERIORES
% --------------------------------------------------------------------------------------------------
nao_altera_linhas_anteriores(Posicoes, L, Ja_Preenchidas):-
	sort(Posicoes, PosicoesOrdenada),
	nao_altera(PosicoesOrdenada, L, Ja_Preenchidas).

% --------------------------------------------------------------------------------------------------
% predicado auxiliar nao_altera
% --------------------------------------------------------------------------------------------------
% nao_altera(P, L, JP) retorna falso se algum dos pares (X,Y) contidos na lista P esta em Jp e tem 
% um valor de X =< a L
% --------------------------------------------------------------------------------------------------
nao_altera(_,1,_):-!.

nao_altera([],_,_).
nao_altera([(L,_)|_], L, _).

nao_altera([H|T], L, Ja_Preenchidas):-
	member(H,Ja_Preenchidas), !, 
	nao_altera(T, L, Ja_Preenchidas).

% --------------------------------------------------------------------------------------------------
% PREDICADO VERIFICA PARCIAL(Puz, Ja_Preenchidas, Dim, Poss)
% --------------------------------------------------------------------------------------------------
verifica_parcial([_, _, Lista_Totais_Colunas], Ja_Preenchidas, Dim, Poss):-
		verifica(Lista_Totais_Colunas, Ja_Preenchidas, Dim, Poss, 0).

% --------------------------------------------------------------------------------------------------
% predicado auxiliar verifica
% --------------------------------------------------------------------------------------------------
% verifica(LTC, JP, D, P, C) de forma iterativa verifica cada uma das D colunas do puzzle fazendo
% uso do contador C. A verificacao e feita consultado a lista dos totais das colunas: LTC, a lista
% de posicoes ja preenchidas JP e a possibilidade de preenchimento P.
% --------------------------------------------------------------------------------------------------
verifica(_, _, _, [], _):-!.

verifica(_, _, [], _, _):-!.

verifica(_,_,Contador,_, Contador):-!. % quando contador de ciclo == Dim

verifica(Lista_Totais_Colunas, Ja_Preenchidas, Dim, Poss, Contador):-
	Contador1 is Contador + 1,
	maximo_nesta_coluna(Lista_Totais_Colunas, Dim, Contador1, Max),
	ja_preenchidas_nesta_coluna(Dim, Ja_Preenchidas,Contador1, Ocupadas),
	remove_lista(Poss,Ja_Preenchidas, Lista_Aux),
	verifica_coluna_N(Lista_Aux, Ocupadas, Max, Contador1),
	verifica(Lista_Totais_Colunas, Ja_Preenchidas, Dim, Poss, Contador1).


% --------------------------------------------------------------------------------------------------
% predicado auxiliar verifica_coluna_N
% --------------------------------------------------------------------------------------------------
% verifica_coluna_N(Poss, Ocupadas, Max, Coluna) para uma possibilidade de 
% preenchimento de linha Poss, vamos verificar se na coluna Coluna, Poss
% respeita Max sabendo que ja sao ocupadas Ocupadas posicoes da coluna Coluna.
% --------------------------------------------------------------------------------------------------
verifica_coluna_N(_, Ocupadas, Max, _):-
	Ocupadas > Max, !,
	fail.

verifica_coluna_N([], _, _, _):- !.

verifica_coluna_N([(_,Y)|T], Ocupadas, Max, Coluna):-
	Y =:= Coluna, !,
	Ocupadas1 is Ocupadas + 1,
	verifica_coluna_N(T, Ocupadas1, Max, Coluna).

verifica_coluna_N([(_,Y)|T], Ocupadas, Max, Coluna):-
	Y =\= Coluna, !,
	verifica_coluna_N(T, Ocupadas, Max, Coluna).

% --------------------------------------------------------------------------------------------------
% predicado auxiliar ja_preenchidas_nesta_coluna
% --------------------------------------------------------------------------------------------------
% ja_preenchidas_nesta_coluna(Dim, Ja_Preenchidas,Num_Coluna,Num) para um dado Puzzle de dimensao 
% Dim, guarda em Num o numero de posicoes ja preenchidas na coluna Num_Coluna
% --------------------------------------------------------------------------------------------------
ja_preenchidas_nesta_coluna(_,_,Num_Coluna,0):-
		Num_Coluna < 0, !.

ja_preenchidas_nesta_coluna(Dim,_,Num_Coluna,0):-
		Num_Coluna > Dim, !.

ja_preenchidas_nesta_coluna(Dim, Ja_Preenchidas,Num_Coluna,Num):-
	ja_preenchidas_nesta_coluna(Dim, Ja_Preenchidas,Num_Coluna,Num, 0).

ja_preenchidas_nesta_coluna(_, [], _, Acc, Acc).

ja_preenchidas_nesta_coluna(_, [(_,Y)|T],Num_Coluna,Num, Acc):-
	Y=:=Num_Coluna, !,
	Acc1 is Acc + 1,
	ja_preenchidas_nesta_coluna(_, T, Num_Coluna, Num, Acc1).

ja_preenchidas_nesta_coluna(_, [(_,Y)|T],Num_Coluna,Num, Acc):-
	Y=\=Num_Coluna, !,
	ja_preenchidas_nesta_coluna(_, T, Num_Coluna, Num, Acc).

% --------------------------------------------------------------------------------------------------
% predicado auxiliar maximo_nesta_coluna
% --------------------------------------------------------------------------------------------------
% maximo_nesta_coluna(Lista_Totais_Colunas, Dim, Num_Coluna, Num) guarda
% em Num o numero maximo de posicoes que se podem preencher
% para a coluna Num_Coluna
% --------------------------------------------------------------------------------------------------
maximo_nesta_coluna(_,_,Num_Coluna,0):-
		Num_Coluna =< 0, !.

maximo_nesta_coluna(_,Dim,Num_Coluna,0):-
		Num_Coluna > Dim, !.

maximo_nesta_coluna(Lista_Totais_Colunas,Dim , Num_Coluna, Num):-
		maximo_nesta_coluna(Lista_Totais_Colunas, Dim, Num_Coluna, Num, 1).

maximo_nesta_coluna([H|_], _, Num_Coluna, H, Num_Coluna):- !.

maximo_nesta_coluna([_|T], Dim, Num_Coluna, Num, Contador):-
		Contador =\= Num_Coluna,
		Contador1 is Contador + 1,
		maximo_nesta_coluna(T, Dim, Num_Coluna,Num, Contador1).


% --------------------------------------------------------------------------------------------------
% PREDICADO POSSIBILIDADES_LINHA
% --------------------------------------------------------------------------------------------------
possibilidades_linha(_,[],_,_,[]).

possibilidades_linha(Puz, Posicoes_linha, Total, Ja_Preenchidas,Poss):-
	linha_primeiro_elemento(Posicoes_linha,L),
	ja_preenchidas_em_L(Ja_Preenchidas, L, Preenchidas_em_L),
	length(Preenchidas_em_L, Num_Preenchidas_em_L),
	Num_Posicoes_a_preencher is Total - Num_Preenchidas_em_L,
	remove_lista(Posicoes_linha, Preenchidas_em_L, Posicoes_Validas),
	findall(Combinacao, gera_combinacoes(Posicoes_Validas, Combinacao), Todas_Combinacoes),
	valida_combinacoes(Todas_Combinacoes, Num_Posicoes_a_preencher, Combinacoes_Validas),
	calcula_dim_puz(Puz,Dim),
	union(Combinacoes_Validas, [[]], Final),
	constroi_lista_Possibilidades(Puz, Ja_Preenchidas, Dim, L, Total, Final, Poss,
								  Preenchidas_em_L, _).

% --------------------------------------------------------------------------------------------------
% predicado auxiliar constroi_lista_Possibilidades
% --------------------------------------------------------------------------------------------------
% funcao auxiliar que calcula a lista de possibilidades para preencher a linha e posteriormente
% ordena a lista antes de unificar em Poss
% --------------------------------------------------------------------------------------------------
constroi_lista_Possibilidades(Puz, Ja_Preenchidas, Dim, L, Total, Combinacoes_Validas, Poss,
							  Preenchidas_em_L, Resultado):-
	%writeln('combinacoes validas:'),
	%writeln(Combinacoes_Validas),
	testa_combinacoes(Puz, Ja_Preenchidas, Dim, L, Total, Combinacoes_Validas, Resultado,
					  Preenchidas_em_L),
	sort(Resultado, Poss).

% --------------------------------------------------------------------------------------------------
% predicado auxiliar calcula_dim_puz
% --------------------------------------------------------------------------------------------------
% calcula a dimensao de um dado puzzle assumindo que o puzzle e quadrado
% --------------------------------------------------------------------------------------------------
calcula_dim_puz([_,List,_], Dim):-
	length(List,Dim).

% --------------------------------------------------------------------------------------------------
% predicado auxiliar testa_combinacoes
% --------------------------------------------------------------------------------------------------
% testa_combinacoes(Puz, Ja_Preenchidas, Dim, Linha, Lista_Todas_Combinacoes, Lista_Poss):
% dadas todas as combinacoes possiveis, determina quais sao as combinacoes,
% que nao violam as regras do puzzle e guarda-as em Lista_Poss
% --------------------------------------------------------------------------------------------------
testa_combinacoes(Puz, Ja_Preenchidas, Dim, Linha, Total, Combinacoes_Validas, Lista_Poss, Extra):-
	testa_combinacoes(Puz, Ja_Preenchidas, Dim, Linha, Total, Combinacoes_Validas, Lista_Poss, [],
					  Extra), !.
% transferencia do acumulador Lista_Aux para a variavel Lista_Poss
testa_combinacoes(_, _, _, _, _, [], Lista_Aux, Lista_Aux, _):- !.

testa_combinacoes(Puz, Ja_Preenchidas, Dim, Linha, Total, [H|T], Lista_Poss, Lista_Aux, Extra):-
	propaga_combinacao(Puz, H, Posicoes),
	propaga_combinacao(Puz, Extra, Extra_Propagado),
	union(Posicoes, Extra_Propagado, Todas_Posicoes),
	ja_preenchidas_em_L(Todas_Posicoes, Linha, Preenchidas_em_L),
	length(Preenchidas_em_L, Num_Preenchidas_em_L),
	Num_Preenchidas_em_L =:= Total,
	verifica_parcial(Puz, Ja_Preenchidas, Dim, Todas_Posicoes),
	nao_altera_linhas_anteriores(Posicoes, Linha, Ja_Preenchidas),
	sort(Todas_Posicoes,Combinacao_Sorted),
	testa_combinacoes(Puz, Ja_Preenchidas, Dim, Linha, Total, T, Lista_Poss,
					  [Combinacao_Sorted|Lista_Aux], Extra).

testa_combinacoes(Puz, Ja_Preenchidas, Dim, Linha, Total, [_|T], Lista_Poss, Lista_Aux, Extra):-
	testa_combinacoes(Puz, Ja_Preenchidas, Dim, Linha, Total, T, Lista_Poss, Lista_Aux, Extra).

% --------------------------------------------------------------------------------------------------
% predicado auxiliar linha_primeiro_elemento
% --------------------------------------------------------------------------------------------------
% linha_primeiro_elemento(P, L) ao receber um lista P de posicoes da forma (X,Y) guarda em L o valor
% de X da cabeca da lista P
% --------------------------------------------------------------------------------------------------
linha_primeiro_elemento([(L,_)|_], L).

% --------------------------------------------------------------------------------------------------
% predicado auxiliar propaga_combinacao
% --------------------------------------------------------------------------------------------------
% propaga_combinacao(Puz, Combinacao, Posicoes) para cada um dos elementos de Combinacao, calcula,
% segundo os termometros de Puz todas as posicoes que sao necessarias preencher para nao violar as
% regras do puzzle, guarda todas as posicoes que Combinacao propaga em Posicoes.
% --------------------------------------------------------------------------------------------------
propaga_combinacao(Puz, Combinacao, Posicoes):-
	propaga_elemento(Puz, Combinacao, Posicoes,[]).

propaga_elemento(_, [], Lista_Aux, Lista_Aux).

propaga_elemento(Puz, [Pos|T], Posicoes,Lista_Aux):-
	propaga(Puz, Pos, Result_Propaga),
	union(Lista_Aux,Result_Propaga, Lista_Aux1),
	propaga_elemento(Puz, T, Posicoes, Lista_Aux1).

% --------------------------------------------------------------------------------------------------
% predicado auxiliar maximo_nesta_linha
% --------------------------------------------------------------------------------------------------
% maximo_nesta_linha(Lista_Totais_Linhas, Dim, Num_Linha, Num) guarda em Num o numero maximo de 
% posicoes que se podem preencher para a linha Num_Linha
% --------------------------------------------------------------------------------------------------
maximo_nesta_linha(_,_,Num_Linha,0):-
		Num_Linha =< 0, !.

maximo_nesta_linha(_,Dim,Num_Linha,0):-
		Num_Linha > Dim, !.

maximo_nesta_linha(Lista_Totais_Linhas,Dim , Num_Linha, Num):-
		maximo_nesta_linha(Lista_Totais_Linhas, Dim, Num_Linha, Num, 1).

maximo_nesta_linha([H|_], _, Num_Linha, H, Num_Linha):- !.

maximo_nesta_linha([_|T], Dim, Num_Linha, Num, Contador):-
		Contador =\= Num_Linha,
		Contador1 is Contador + 1,
		maximo_nesta_linha(T, Dim, Num_Linha,Num, Contador1).

% --------------------------------------------------------------------------------------------------
% predicado auxiliar ja_preenchidas_em_L
% --------------------------------------------------------------------------------------------------
% ja_preenchidas_em_L(Ja_Preenchidas, L, Lista_Resultado): guarda em Lista_Resultado todas as 
% posicoes de Ja_Preenchidas da linha L.
% --------------------------------------------------------------------------------------------------
ja_preenchidas_em_L(Ja_Preenchidas, L, Lista_Resultado):-
	ja_preenchidas_em_L(Ja_Preenchidas, L, Lista_Resultado, []).

ja_preenchidas_em_L([], _, Lista_Aux, Lista_Aux):-!.

ja_preenchidas_em_L([(X,Y)|T], L, Lista_Resultado, Lista_Aux):-
	X =:= L, !,
	ja_preenchidas_em_L(T, L, Lista_Resultado, [(X,Y)|Lista_Aux]).

ja_preenchidas_em_L([(X,_)|T], L, Lista_Resultado, Lista_Aux):-
	X =\= L, !,
	ja_preenchidas_em_L(T, L, Lista_Resultado, Lista_Aux).

% --------------------------------------------------------------------------------------------------
% predicado auxiliar remove_lista
% --------------------------------------------------------------------------------------------------
% remove_lista(L1, L2, L) guarda em L o resultado de L1 - (L1 intercecao L2)
% --------------------------------------------------------------------------------------------------
remove_lista([], _, []).

remove_lista([H|T], L2, ListaFinal):-
	member(H, L2), !,
	remove_lista(T, L2, ListaFinal). 

remove_lista([H|T], L2, [H|ListaFinal]):-
	remove_lista(T, L2, ListaFinal).

% --------------------------------------------------------------------------------------------------
% predicado auxiliar valida_combinacoes
% --------------------------------------------------------------------------------------------------
% Dado um conjunto de Combinacoes: Lista_Todas_Combinacoes e um total Total, guarda em 
% Lista_Combinacoes_Validas aquelas que tem um numero de elementos =< a Total
% --------------------------------------------------------------------------------------------------
valida_combinacoes(Lista_Todas_Combinacoes, Total, Lista_Combinacoes_Validas):-
	valida_combinacoes(Lista_Todas_Combinacoes, Total,
					   Lista_Combinacoes_Validas, []).

valida_combinacoes([], _, Lista_Aux, Lista_Aux).

valida_combinacoes([H|T], Total, Lista_Combinacoes_Validas, Lista_Aux):-
	length(H, Size),
	Size =< Total, !,
	valida_combinacoes(T, Total, Lista_Combinacoes_Validas, [H|Lista_Aux]).

valida_combinacoes([H|T], Total, Lista_Combinacoes_Validas, Lista_Aux):-
	length(H, Size),
	Size > Total, !,
	valida_combinacoes(T, Total, Lista_Combinacoes_Validas, Lista_Aux).

% --------------------------------------------------------------------------------------------------
% predicado auxiliar gera_combinacoes
% --------------------------------------------------------------------------------------------------
% gera_combinacoes(L1, L) guarda em L um combinacao possivel dos elementos de L1 chamadas sucessivas
% levam a combinacoes diferentes ate que ja nao haja mais combinacoes possiveis
% --------------------------------------------------------------------------------------------------
gera_combinacoes(_,[]).

gera_combinacoes([X|T],[X|Comb]):-
	gera_combinacoes(T,Comb).

gera_combinacoes([_|T],[X|Comb]):-
	gera_combinacoes(T,[X|Comb]).

% --------------------------------------------------------------------------------------------------
% PREDICADO RESOLVE
% --------------------------------------------------------------------------------------------------
resolve(Puz, Solucao):-
	calcula_dim_puz(Puz, Dim),
	Limite is Dim + 1,
	res(Puz, [], Aux, 1, Dim, Limite),
	sort(Aux, Solucao).

% --------------------------------------------------------------------------------------------------
% predicado auxiliar res
% --------------------------------------------------------------------------------------------------
% res(Puz, Ja_Preenchidas, Solucao, Linha_Actual, Dim, Limite) resolve o puzzle Puz, guardando a
% solucao em Solucao. Dim, Limite e Linha actual sao mantidos para poder determinar quando terminar
% o calculo da solucao. 
% --------------------------------------------------------------------------------------------------
res(_, Ja_Preenchidas, Ja_Preenchidas, Limite, _, Limite):-!.
% quando linha actual = limite ja preenchemos correctamente todas as posicoes do puzzle entao
% transferimos Ja_Preenchidas para Solucao

res(Puz, Ja_Preenchidas, Solucao, Linha_Actual, Dim, Limite):-

	get_linha_x_puz(Puz,Linha_Actual,Posicoes_linha),
	get_totais_linhas(Puz, TotaisLinhas),
	maximo_nesta_linha(TotaisLinhas, Dim, Linha_Actual, Total),
	possibilidades_linha(Puz, Posicoes_linha, Total,Ja_Preenchidas, Poss_L),
	explora_possibilidades(Puz, Ja_Preenchidas, Solucao, Linha_Actual,Dim, Limite, Poss_L).

explora_possibilidades(_, _, _, _, _, _, _, _, []).

explora_possibilidades(Puzz, Ja_Preenchidas, Solucao, Linha, Dim, Limite, [H|_]):-
	Linha1 is Linha + 1,
	union(Ja_Preenchidas, H, Ja_Preenchidas_Nova),
	res(Puzz, Ja_Preenchidas_Nova, Solucao, Linha1, Dim, Limite),!.

explora_possibilidades(Puzz, Ja_Preenchidas, Solucao, Linha, Dim, Limite, [_|T]):-
	explora_possibilidades(Puzz, Ja_Preenchidas, Solucao, Linha, Dim, Limite, T).

% --------------------------------------------------------------------------------------------------
% predicado auxiliar remove_head
% --------------------------------------------------------------------------------------------------
% remove_head(L, H, T) separa um lista L na sua head H e no seu tail T
% --------------------------------------------------------------------------------------------------
remove_head([H|T], H, T).

% --------------------------------------------------------------------------------------------------
% predicado auxiliar get_linha_x_puz
% --------------------------------------------------------------------------------------------------
% get_linha_x_puz(P, X, Linha):- devolve todas as posicoes preenchiveis da linha X do puzzle P
% --------------------------------------------------------------------------------------------------
get_linha_x_puz([H|_], X, Linha):-
	get_linha_x_puz(H, X, Aux, []),
	sort(Aux, Linha).

get_linha_x_puz([], _, Acc, Acc):-!.

get_linha_x_puz([[]|T], L, Result, Acc):-
	get_linha_x_puz(T, L, Result, Acc).

get_linha_x_puz([[(L,C)|T]|OutrosTerm], L, Result, Acc):-
	get_linha_x_puz([T|OutrosTerm], L, Result, [(L,C)|Acc]), !.

get_linha_x_puz([[(_,_)|T]|OutrosTerm], L, Result, Acc):-
	get_linha_x_puz([T|OutrosTerm], L, Result, Acc), !.

% --------------------------------------------------------------------------------------------------
% predicado auxiliar get_totais_linhas
% --------------------------------------------------------------------------------------------------
% get_linha_x_puz(P, TL):- devolve em TL a lista de totais das linhas do puzzle P
% --------------------------------------------------------------------------------------------------
get_totais_linhas([_, TL, _], TL).
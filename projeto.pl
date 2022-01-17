% 103124 - Goncalo Sampaio Barias - goncalo.barias@tecnico.ulisboa.pt

% -------------------------------------------------------------------- %
%                                                                      %
%                  Solucionador de Puzzles Hashi                       %
%                                                                      %
%  Projeto - Logica para Programacao 21/22                             %
%  Licenciatura em Engenharia Informatica e de Computadores (Alameda)  %
%  Instituto Superior Tecnico                                          %
%                                                                      %
% -------------------------------------------------------------------- %

% ------------------------------------------------------------------------------
% extrai_ilhas_linha(N_lin, Lin, Ilhas)
% ------------------------------------------------------------------------------

extrai_ilhas_linha(N_lin, Lin, Ilhas) :-
  findall(
    ilha(N_Pontes, (N_lin, N_col)), (nth1(N_col, Lin, N_Pontes),
    N_Pontes \== 0), Ilhas_aux
  ),
  sort(2, @<, Ilhas_aux, Ilhas).

% ------------------------------------------------------------------------------
% ilhas(Puz, Ilhas)
% ------------------------------------------------------------------------------

ilhas(Puz, Ilhas) :-
  findall(
    Ilha, (nth1(N_lin, Puz, Lin), extrai_ilhas_linha(N_lin, Lin, Ilhas_aux),
    member(Ilha, Ilhas_aux)), Ilhas
  ).

% ------------------------------------------------------------------------------
% adjacente(Adj, El0, El1)
% Predicado auxiliar
% ------------------------------------------------------------------------------

adjacente(Adj, El0, El1) :-
  append(_, [El0,El1|_], Adj) ;
  append(_, [El1,El0|_], Adj).

% ------------------------------------------------------------------------------
% vizinhas(Ilhas, Ilha, Vz)
% ------------------------------------------------------------------------------

vizinhas(Ilhas, Ilha, Vz) :-
  ilha(_, (N_l, N_c)) = Ilha,
  findall(
    Ilha_viz, (member(Ilha_viz, Ilhas),
    Ilha_viz = ilha(_, (N_l, _))), Lin_aux
  ), include(adjacente(Lin_aux, Ilha), Lin_aux, Lin),
  findall(
    Ilha_viz, (member(Ilha_viz, Ilhas),
    Ilha_viz = ilha(_, (_, N_c))), Col_aux
  ), include(adjacente(Col_aux, Ilha), Col_aux, Col),
  append(Lin, Col, Vz_aux),
  sort(2, @=<, Vz_aux, Vz).

% ------------------------------------------------------------------------------
% estado(Ilhas, Estado)
% ------------------------------------------------------------------------------

estado(Ilhas, Estado) :-
  findall(
    [X, Y, []], (member(X, Ilhas), vizinhas(Ilhas, X, Y)),
    Estado
  ).

% ------------------------------------------------------------------------------
% entre(Pos1, Pos2, Posicoes)
% Predicado auxiliar
% ------------------------------------------------------------------------------

entre(Pos1, Pos2, Posicoes) :-
  sort([Pos1, Pos2], [Pos1_novo, Pos2_novo]),
  Pos1_aux is Pos1_novo + 1, Pos2_aux is Pos2_novo - 1,
  between(Pos1_aux, Pos2_aux, Posicoes).

% ------------------------------------------------------------------------------
% posicoes_entre(Pos1, Pos2, Posicoes)
% ------------------------------------------------------------------------------

posicoes_entre((Pos1_X, Pos1_Y), (Pos2_X, Pos2_Y), Posicoes) :-
  findall(
    Pos,
    (Pos1_X == Pos2_X, entre(Pos1_Y, Pos2_Y, N), Pos = (Pos1_X, N) ;
    Pos1_Y == Pos2_Y, entre(Pos1_X, Pos2_X, N), Pos = (N, Pos2_Y)),
    Posicoes
  ), Posicoes \== [].

% ------------------------------------------------------------------------------
% cria_ponte(Pos1, Pos2, Ponte)
% ------------------------------------------------------------------------------

cria_ponte(Pos1, Pos2, ponte(Pos1_novo, Pos2_novo)) :-
  sort([Pos1, Pos2], [Pos1_novo, Pos2_novo]).

% ------------------------------------------------------------------------------
% caminho_livre(Pos1, Pos2, Posicoes, I, Vz)
% ------------------------------------------------------------------------------

caminho_livre(Pos1, Pos2, Posicoes, ilha(_, PosI), ilha(_, PosVz)) :-
  posicoes_entre(PosI, PosVz, PosEntre),
  findall(
    Pos, (member(Pos, PosEntre), subset([Pos], Posicoes)),
    Posicoes_comum
  ), length(Posicoes_comum, Len),
  (Len \== 1 ; lists:perm([Pos1, Pos2], [PosI, PosVz])).

% ------------------------------------------------------------------------------
% actualiza_vizinhas_entrada(Pos1, Pos2, Posicoes, Entrada, Nova_Entrada)
% ------------------------------------------------------------------------------

actualiza_vizinhas_entrada(Pos1, Pos2, Posicoes, [I, Vz, Pontes], [I, Vz_novo, Pontes]) :-
  findall(
    Pos, (member(Pos, Vz),
    caminho_livre(Pos1, Pos2, Posicoes, I, Pos)),
    Vz_aux
  ),
  sort(2, @<, Vz_aux, Vz_novo), ! ; Vz_novo = [].

% ------------------------------------------------------------------------------
% actualiza_vizinhas_apos_pontes(Estado, Pos1, Pos2, Novo_estado)
% ------------------------------------------------------------------------------

actualiza_vizinhas_apos_pontes(Estado, Pos1, Pos2, Novo_estado) :-
  posicoes_entre(Pos1, Pos2, Posicoes),
  maplist(
    actualiza_vizinhas_entrada(Pos1, Pos2, Posicoes),
    Estado, Novo_estado
  ).

% ------------------------------------------------------------------------------
% ilhas_terminadas(Estado, Ilhas_term)
% ------------------------------------------------------------------------------

ilhas_terminadas(Estado, Ilhas_term) :-
  findall(
    Ilha, (member([Ilha, _, Pontes], Estado), Ilha = ilha(N_Pontes, _),
    N_Pontes \== 'X', length(Pontes, N_Pontes)), Ilhas_term
  ).

% ------------------------------------------------------------------------------
% tira_ilhas_terminadas_entrada(Ilhas_term, Entrada, Nova_Entrada)
% ------------------------------------------------------------------------------

tira_ilhas_terminadas_entrada(Ilhas_term, [I, Vz, Pontes], [I, Vz_novo, Pontes]) :-
  subtract(Vz, Ilhas_term, Vz_novo).

% ------------------------------------------------------------------------------
% tira_ilhas_terminadas(Estado, Ilhas_term, Novo_estado)
% ------------------------------------------------------------------------------

tira_ilhas_terminadas(Estado, Ilhas_term, Novo_estado) :-
  maplist(
    tira_ilhas_terminadas_entrada(Ilhas_term),
    Estado, Novo_estado
  ).

% ------------------------------------------------------------------------------
% marca_ilhas_terminadas_entrada(Ilhas_term, Entrada, Nova_Entrada)
% ------------------------------------------------------------------------------

marca_ilhas_terminadas_entrada(Ilhas_term, [Ilha, Vz, Pontes], [Nova_Ilha, Vz, Pontes]) :-
  Ilha = ilha(_, (N_lin, N_col)),
  (subset([Ilha], Ilhas_term), Nova_Ilha = ilha('X', (N_lin, N_col)) ;
  Nova_Ilha = Ilha).

% ------------------------------------------------------------------------------
% marca_ilhas_terminadas(Estado, Ilhas_term, Novo_estado)
% ------------------------------------------------------------------------------

marca_ilhas_terminadas(Estado, Ilhas_term, Novo_estado) :-
  maplist(
    marca_ilhas_terminadas_entrada(Ilhas_term),
    Estado, Novo_estado
  ).

% ------------------------------------------------------------------------------
% trata_ilhas_terminadas(Estado, Novo_estado)
% ------------------------------------------------------------------------------

trata_ilhas_terminadas(Estado, Novo_estado) :-
  ilhas_terminadas(Estado, Ilhas_term),
  tira_ilhas_terminadas(Estado, Ilhas_term, Estado_aux),
  marca_ilhas_terminadas(Estado_aux, Ilhas_term, Novo_estado).

% ------------------------------------------------------------------------------
% adiciona_pontes(Num_pontes, Ilha1, Ilha2, Entrada, Nova_Entrada)
% Predicado auxiliar
% ------------------------------------------------------------------------------

adiciona_pontes(Num_pontes, Ilha1, Ilha2, Entrada, Nova_Entrada) :-
  Ilha1 = ilha(_, Pos1), Ilha2 = ilha(_, Pos2),
  Entrada = [Ilha, Vzs, Pontes_antigas],
  cria_ponte(Pos1, Pos2, Ponte_aux),
  (Num_pontes == 1, Ponte = [Ponte_aux] ;
  Num_pontes == 2, append([Ponte_aux], [Ponte_aux], Ponte)),
  (Ilha == Ilha1, append(Pontes_antigas, Ponte, Pontes) ;
  Ilha == Ilha2, append(Pontes_antigas, Ponte, Pontes) ;
  Pontes = Pontes_antigas),
  Nova_Entrada = [Ilha, Vzs, Pontes].

% ------------------------------------------------------------------------------
% junta_pontes(Estado, Num_pontes, Ilha1, Ilha2, Novo_estado)
% ------------------------------------------------------------------------------

junta_pontes(Estado, Num_pontes, Ilha1, Ilha2, Novo_estado) :-
  Ilha1 = ilha(_, Pos1), Ilha2 = ilha(_, Pos2),
  maplist(
    adiciona_pontes(Num_pontes, Ilha1, Ilha2),
    Estado, Estado_aux
  ), !,
  actualiza_vizinhas_apos_pontes(Estado_aux, Pos1, Pos2, Novo_estado_aux),
  trata_ilhas_terminadas(Novo_estado_aux, Novo_estado).

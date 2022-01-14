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

:- [codigo_comum].

% ------------------------------------------------------------------------------
% extrai_ilhas_linha(N_lin, Lin, Ilhas)
% ------------------------------------------------------------------------------

extrai_ilhas_linha(N_lin, Lin, Ilhas) :-
  setof(
    ilha(Ponte, (N_lin, N_col)), (nth1(N_col, Lin, Ponte),
    Ponte \== 0), Ilhas_aux
  ),
  sort(2, @=<, Ilhas_aux, Ilhas); Ilhas = [].

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
% ------------------------------------------------------------------------------

adjacente(Adj, El0, El1) :-
  append(_, [El0,El1|_], Adj);
  append(_, [El1,El0|_], Adj).

% ------------------------------------------------------------------------------
% vizinhas(Ilhas, Ilha, Viz)
% ------------------------------------------------------------------------------

vizinhas(Ilhas, Ilha, Viz) :-
  ilha(_, (N_l, N_c)) = Ilha,
  findall(
    Ilha_viz, (member(Ilha_viz, Ilhas),
    Ilha_viz = ilha(_, (N_L, _)), N_L == N_l), Lin_aux
  ), include(adjacente(Lin_aux, Ilha), Lin_aux, Lin),
  findall(
    Ilha_viz, (member(Ilha_viz, Ilhas),
    Ilha_viz = ilha(_, (_, N_C)), N_C == N_c), Col_aux
  ), include(adjacente(Col_aux, Ilha), Col_aux, Col),
  append(Lin, Col, Viz_aux),
  sort(2, @=<, Viz_aux, Viz).

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
% ------------------------------------------------------------------------------

entre(Pos1, Pos2, Posicoes) :-
  sort([Pos1, Pos2], [Pos1_novo, Pos2_novo]),
  Pos1_aux is Pos1_novo + 1, Pos2_aux is Pos2_novo - 1,
  between(Pos1_aux, Pos2_aux, Posicoes).

% ------------------------------------------------------------------------------
% posicoes_entre(Pos1, Pos2, Posicoes)
% ------------------------------------------------------------------------------

posicoes_entre(Pos1, Pos2, Posicoes) :-
  findall(
    Pos, (Pos1 = (Pos1_X, Pos1_Y), Pos2 = (Pos2_X, Pos2_Y),
    (Pos1_X == Pos2_X, entre(Pos1_Y, Pos2_Y, N), Pos = (Pos1_X, N) ;
    Pos1_Y == Pos2_Y, entre(Pos1_X, Pos2_X, N), Pos = (N, Pos2_Y))),
    Posicoes
  ), Posicoes \== [].

% ------------------------------------------------------------------------------
% cria_ponte(Pos1, Pos2, Ponte)
% ------------------------------------------------------------------------------

cria_ponte(Pos1, Pos2, Ponte) :-
  sort([Pos1, Pos2], [Pos1_novo, Pos2_novo]),
  Ponte = ponte(Pos1_novo, Pos2_novo).

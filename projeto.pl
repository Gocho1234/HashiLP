% -------------------------------------------------------------------- %
%                                                                      %
%                         Hashi                                        %
%                                                                      %
%  Projeto - Logica para Programacao 21/22                             %
%  Licenciatura em Engenharia Informatica e de Computadores (Alameda)  %
%  Instituto Superior Tecnico                                          %
%                                                                      %
% -------------------------------------------------------------------- %

:- [codigo_comum, puzzles_publicos].

% ------------------------------------------------------------------------------
% extrai_ilhas_linha(N_lin, Lin, Ilhas)
% ------------------------------------------------------------------------------

extrai_ilhas_linha(N_lin, Lin, Ilhas) :-
  setof(
    ilha(Ponte, (N_lin, N_col)), (member(Ponte, Lin), Ponte \== 0,
    nth1(N_col, Lin, Ponte)), Ilhas_aux
  ),
  sort(2, @=<, Ilhas_aux, Ilhas).

% ------------------------------------------------------------------------------
% ilhas(Puz, Ilhas)
% ------------------------------------------------------------------------------

ilhas(Puz, Ilhas) :-
  findall(
    Ilha, (member(Lin, Puz), nth1(N_lin, Puz, Lin),
    extrai_ilhas_linha(N_lin, Lin, Ilha)), Ilhas_aux
  ),
  flatten(Ilhas_aux, Ilhas).

% ------------------------------------------------------------------------------
% vizinhas(Ilhas, Ilha, Vizinhas)
% ------------------------------------------------------------------------------

vizinhas(Ilhas, Ilha, Viz) :-
  ilha(_, (N_l, N_c)) = Ilha,
  findall(
    Ilha_viz, (member(Ilha_viz, Ilhas), Ilha_viz = ilha(_, (N_L, _)),
    N_L == N_l), Lin_aux
  ),
  findall(
    Ilha_viz, (member(Ilha_viz, Ilhas), Ilha_viz = ilha(_, (_, N_C)),
    N_C == N_c), Col_aux
  ),
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

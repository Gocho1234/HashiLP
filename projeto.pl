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

% ------------------------------------ 2.1 -------------------------------------
% extrai_ilhas_linha(N_lin, Lin, Ilhas)
% extrai_ilhas_linha/3: Retira as respetivas ilhas (todas as entradas com
% valor diferente de 0) de uma dada linha, e apresenta-as de forma organizada
% da esquerda para a direita.
% ------------------------------------------------------------------------------

extrai_ilhas_linha(N_lin, Lin, Ilhas) :-
  findall(
    ilha(N_Pontes, (N_lin, N_col)), (nth1(N_col, Lin, N_Pontes),
    N_Pontes \== 0), Ilhas
  ).

% ------------------------------------ 2.2 -------------------------------------
% ilhas(Puz, Ilhas)
% ilhas/2: Percorre cada linha de um puzzle e usa o predicado
% extrai_ilhas_linha/3 para extrair todas as ilhas. No fim, apresenta-as de
% forma organizada da esquerda para a direita e de cima para baixo.
% ------------------------------------------------------------------------------

ilhas(Puz, Ilhas) :-
  findall(
    Ilha, (nth1(N_lin, Puz, Lin), extrai_ilhas_linha(N_lin, Lin, Ilhas_aux),
    member(Ilha, Ilhas_aux)), Ilhas
  ).

% ---------------------------------- Auxiliar ----------------------------------
% adjacente(Lista, El0, El1)
% adjacente/3: Verifica se numa dada lista dois elementos (El0 e El1) sao
% adjacentes.
% ------------------------------------------------------------------------------

adjacente(Lista, El0, El1) :-
  append(_, [El0,El1|_], Lista) ;
  append(_, [El1,El0|_], Lista).

% ------------------------------------ 2.3 -------------------------------------
% vizinhas(Ilhas, Ilha, Vzs)
% vizinhas/3: A um dado conjunto de ilhas extrai aquelas que sao vizinhas da
% ilha fornecida. Obtem inicialmente todas as ilhas que estao na mesma coluna ou
% linha que a ilha dada e depois vai buscar as ilhas que teem um caminho livre
% entre elas e a ilha fornecida, utilizando um predicado auxiliar adjacente/3
% para esse efeito.
% ------------------------------------------------------------------------------

vizinhas(Ilhas, Ilha, Vzs) :-
  ilha(_, (N_lin, N_col)) = Ilha,
  findall(
    Ilha_vz, (member(Ilha_vz, Ilhas),
    Ilha_vz = ilha(_, (N_lin, _))), Lin_aux
  ), include(adjacente(Lin_aux, Ilha), Lin_aux, Lin),
  findall(
    Ilha_vz, (member(Ilha_vz, Ilhas),
    Ilha_vz = ilha(_, (_, N_col))), Col_aux
  ), include(adjacente(Col_aux, Ilha), Col_aux, Col),
  append(Lin, Col, Vzs_aux),
  sort(2, @=<, Vzs_aux, Vzs).

% ------------------------------------ 2.4 -------------------------------------
% estado(Ilhas, Estado)
% estado/2: Obtem o estado de todas as ilhas de um dado puzzle, ou seja, vai
% buscar todas as ilhas vizinhas a essa ilha e guarda-as junto da ilha
% fornecida numa entrada. Por fim, o terceiro elemento da entrada inicia sempre
% com uma lista vazia.
% ------------------------------------------------------------------------------

estado(Ilhas, Estado) :-
  findall(
    [Ilha, Vzs, []], (member(Ilha, Ilhas), vizinhas(Ilhas, Ilha, Vzs)),
    Estado
  ).

% ---------------------------------- Auxiliar ----------------------------------
% entre(Pos1, Pos2, Posicoes)
% entre/3: Um predicado semelhante ao between, mas que ignora a monotonia entre
% os valores nos primeiros dois argumentos e nao os inclui no resultado final.
% ------------------------------------------------------------------------------

entre(Pos1, Pos2, Posicoes) :-
  sort([Pos1, Pos2], [Pos1_aux, Pos2_aux]),
  Pos1_novo is Pos1_aux + 1, Pos2_novo is Pos2_aux - 1,
  between(Pos1_novo, Pos2_novo, Posicoes).

% ------------------------------------ 2.5 -------------------------------------
% posicoes_entre(Pos1, Pos2, Posicoes)
% posicoes_entre/3: Verifica se as duas posicoes que recebe estao na mesma linha
% ou coluna e depois utiliza o predicado entre/3 para gerar as posicoes entre
% essas duas posicoes.
% ------------------------------------------------------------------------------

posicoes_entre((Pos1_X, Pos1_Y), (Pos2_X, Pos2_Y), Posicoes) :-
  findall(
    Pos,
    (Pos1_X == Pos2_X, entre(Pos1_Y, Pos2_Y, Y), Pos = (Pos1_X, Y) ;
    Pos1_Y == Pos2_Y, entre(Pos1_X, Pos2_X, X), Pos = (X, Pos2_Y)),
    Posicoes
  ), Posicoes \== [].

% ------------------------------------ 2.6 -------------------------------------
% cria_ponte(Pos1, Pos2, Ponte)
% cria_ponte/3: Organiza as duas posicoes pela ordem do puzzle (esquerda-direita
% e cima-baixo). Por fim, formata as duas posicoes dentro de uma ponte,
% retornando-a no fim.
% ------------------------------------------------------------------------------

cria_ponte(Pos1, Pos2, ponte(Pos1_novo, Pos2_novo)) :-
  sort([Pos1, Pos2], [Pos1_novo, Pos2_novo]).

% ------------------------------------ 2.7 -------------------------------------
% caminho_livre(Pos1, Pos2, Posicoes, I, Vz)
% caminho_livre/5: Verifica se a ilha Vz continua a ser vizinha da ilha I apos
% a adicao de uma ponte entre as ilhas nas posicoes Pos1 e Pos2. Para este
% efeito, obtem as posicoes em comum entre Posicoes e as posicoes entre as
% ilhas I e Vz. Se apenas houver uma posicao em comum entao nao ha caminho livre
% desde que as ilhas I e Vz sejam diferentes das ilhas nas Pos1 e Pos2. Caso
% haja mais que uma ou nenhuma posicao em comum entao ha sempre caminho livre.
% ------------------------------------------------------------------------------

caminho_livre(Pos1, Pos2, Posicoes, ilha(_, PosI), ilha(_, PosVz)) :-
  posicoes_entre(PosI, PosVz, PosEntre),
  findall(
    Pos, (member(Pos, PosEntre), subset([Pos], Posicoes)),
    Posicoes_comum
  ), length(Posicoes_comum, Len),
  (Len \== 1 ; lists:perm([Pos1, Pos2], [PosI, PosVz])).

% ------------------------------------ 2.8 -------------------------------------
% actualiza_vizinhas_entrada(Pos1, Pos2, Posicoes, Entrada, Nova_Entrada)
% actualiza_vizinhas_entrada/5: Apos a adicao de uma ponte entre as ilhas nas
% posicoes Pos1 e Pos2 este predicado atualiza uma entrada, ou seja, remove
% todas as ilhas que deixaram de ser vizinhas devido a essa nova ponte.
% ------------------------------------------------------------------------------

actualiza_vizinhas_entrada(Pos1, Pos2, Posicoes, [I, Vzs, Pontes], [I, Vzs_novo, Pontes]) :-
  findall(
    Vz, (member(Vz, Vzs), caminho_livre(Pos1, Pos2, Posicoes, I, Vz)),
    Vzs_aux
  ),
  sort(2, @<, Vzs_aux, Vzs_novo), ! ; Vzs_novo = [].

% ------------------------------------ 2.9 -------------------------------------
% actualiza_vizinhas_apos_pontes(Estado, Pos1, Pos2, Novo_estado)
% actualiza_vizinhas_apos_pontes/4: Utiliza o predicado
% actualiza_vizinhas_entrada/5 para atualizar todas as entradas de um
% determinado estado.
% ------------------------------------------------------------------------------

actualiza_vizinhas_apos_pontes(Estado, Pos1, Pos2, Novo_estado) :-
  posicoes_entre(Pos1, Pos2, Posicoes),
  maplist(
    actualiza_vizinhas_entrada(Pos1, Pos2, Posicoes),
    Estado, Novo_estado
  ).

% ------------------------------------ 2.10 ------------------------------------
% ilhas_terminadas(Estado, Ilhas_term)
% ilhas_terminadas/2: Percorre todas as entradas de um determinado estado e
% extrai todas as ilhas que estao terminadas. Assim, verifica todas as entradas
% que teem um numero de pontes adequado e depois guarda as corretas.
% ------------------------------------------------------------------------------

ilhas_terminadas(Estado, Ilhas_term) :-
  findall(
    Ilha, (member([Ilha, _, Pontes], Estado), Ilha = ilha(N_Pontes, _),
    N_Pontes \== 'X', length(Pontes, N_Pontes)), Ilhas_term
  ).

% ------------------------------------------------------------------------------
% tira_ilhas_terminadas_entrada(Ilhas_term, Entrada, Nova_Entrada)
% ------------------------------------------------------------------------------

tira_ilhas_terminadas_entrada(Ilhas_term, [I, Vzs, Pontes], [I, Vzs_novo, Pontes]) :-
  subtract(Vzs, Ilhas_term, Vzs_novo).

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

marca_ilhas_terminadas_entrada(Ilhas_term, [Ilha, Vzs, Pontes], [Nova_Ilha, Vzs, Pontes]) :-
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
% adiciona_pontes(N_Pontes, Ilha1, Ilha2, Entrada, Nova_Entrada)
% Predicado auxiliar
% ------------------------------------------------------------------------------

adiciona_pontes(N_Pontes, Ilha1, Ilha2, Entrada, Nova_Entrada) :-
  Ilha1 = ilha(_, Pos1), Ilha2 = ilha(_, Pos2),
  Entrada = [Ilha, Vzs, Pontes_antigas],
  cria_ponte(Pos1, Pos2, Ponte_aux),
  (N_Pontes == 1, Ponte = [Ponte_aux] ;
  N_Pontes == 2, append([Ponte_aux], [Ponte_aux], Ponte)),
  (Ilha == Ilha1, append(Pontes_antigas, Ponte, Pontes) ;
  Ilha == Ilha2, append(Pontes_antigas, Ponte, Pontes) ;
  Pontes = Pontes_antigas),
  Nova_Entrada = [Ilha, Vzs, Pontes].

% ------------------------------------------------------------------------------
% junta_pontes(Estado, N_Pontes, Ilha1, Ilha2, Novo_estado)
% ------------------------------------------------------------------------------

junta_pontes(Estado, N_Pontes, Ilha1, Ilha2, Novo_estado) :-
  Ilha1 = ilha(_, Pos1), Ilha2 = ilha(_, Pos2),
  maplist(
    adiciona_pontes(N_Pontes, Ilha1, Ilha2),
    Estado, Estado_aux
  ), !,
  actualiza_vizinhas_apos_pontes(Estado_aux, Pos1, Pos2, Novo_estado_aux),
  trata_ilhas_terminadas(Novo_estado_aux, Novo_estado).

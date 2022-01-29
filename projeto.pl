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
    ilha(N_Pontes, (N_lin, N_col)),
    (nth1(N_col, Lin, N_Pontes), N_Pontes \== 0),
    Ilhas
  ).

% ------------------------------------ 2.2 -------------------------------------
% ilhas(Puz, Ilhas)
% ilhas/2: Percorre cada linha de um puzzle e usa o predicado
% extrai_ilhas_linha/3 para extrair todas as ilhas. No fim, apresenta-as de
% forma organizada da esquerda para a direita e de cima para baixo.
% ------------------------------------------------------------------------------

ilhas(Puz, Ilhas) :-
  findall(
    Ilha,
    (nth1(N_lin, Puz, Lin), extrai_ilhas_linha(N_lin, Lin, Ilhas_aux),
    member(Ilha, Ilhas_aux)),
    Ilhas
  ).

% ---------------------------------- Auxiliar ----------------------------------
% adjacente(Lista, El1, El2)
% adjacente/3: Verifica se numa dada lista dois elementos (El1 e El2) sao
% adjacentes. Para este efeito verificamos se os elementos aparecem juntos na
% lista fornecida atraves do append/3.
% ------------------------------------------------------------------------------

adjacente(Lista, El1, El2) :-
  append(_, [El1,El2|_], Lista) ;
  append(_, [El2,El1|_], Lista).

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
  % Obtem todas as ilhas vizinhas na mesma linha que a ilha fornecida.
  findall(
    Ilha_vz,
    (member(Ilha_vz, Ilhas), Ilha_vz = ilha(_, (N_lin, _))),
    Lin_aux
  ),
  include(adjacente(Lin_aux, Ilha), Lin_aux, Lin),
  % Obtem todas as ilhas vizinhas na mesma coluna que a ilha fornecida.
  findall(
    Ilha_vz,
    (member(Ilha_vz, Ilhas), Ilha_vz = ilha(_, (_, N_col))),
    Col_aux
  ),
  include(adjacente(Col_aux, Ilha), Col_aux, Col),
  append(Lin, Col, Vzs_aux),
  sort(2, @=<, Vzs_aux, Vzs).

% ------------------------------------ 2.4 -------------------------------------
% estado(Ilhas, Estado)
% estado/2: Obtem o estado de todas as ilhas de um dado puzzle, ou seja, vai
% buscar todas as ilhas vizinhas a essa ilha e guarda-as junto da ilha
% fornecida numa entrada (lista com 3 elementos). Por fim, o terceiro elemento
% da entrada inicia sempre com uma lista vazia.
% ------------------------------------------------------------------------------

estado(Ilhas, Estado) :-
  findall(
    [Ilha, Vzs, []],
    (member(Ilha, Ilhas), vizinhas(Ilhas, Ilha, Vzs)),
    Estado
  ).

% ---------------------------------- Auxiliar ----------------------------------
% entre(Val1, Val2, ValEntre)
% entre/3: Um predicado semelhante ao between, mas que ignora a monotonia entre
% os valores nos primeiros dois argumentos e nao os inclui no resultado final.
% ------------------------------------------------------------------------------

entre(Val1, Val2, ValEntre) :-
  sort([Val1, Val2], [Val1_aux, Val2_aux]),
  Val1_novo is Val1_aux + 1, Val2_novo is Val2_aux - 1,
  between(Val1_novo, Val2_novo, ValEntre).

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
  ),
  Posicoes \== [].

% ------------------------------------ 2.6 -------------------------------------
% cria_ponte(Pos1, Pos2, Ponte)
% cria_ponte/3: Organiza as duas posicoes pela ordem do puzzle (esquerda-direita
% e cima-baixo). Por fim, formata as duas posicoes dentro de uma ponte,
% retornando-a no final.
% ------------------------------------------------------------------------------

cria_ponte(Pos1, Pos2, ponte(Pos1_novo, Pos2_novo)) :-
  sort([Pos1, Pos2], [Pos1_novo, Pos2_novo]).

% ------------------------------------ 2.7 -------------------------------------
% caminho_livre(Pos1, Pos2, Posicoes, I, Vz)
% caminho_livre/5: Verifica se a ilha Vz continua a ser vizinha da ilha I apos
% a adicao de uma ponte entre as ilhas nas posicoes Pos1 e Pos2. Para este
% efeito, obtem as posicoes partilhadas entre Posicoes e as posicoes entre as
% ilhas I e Vz. Se apenas houver uma posicao em comum entao nao ha caminho livre
% desde que as ilhas I e Vz sejam diferentes das ilhas nas Pos1 e Pos2. Caso
% haja mais que uma ou nenhuma posicao em comum entao ha sempre caminho livre.
% ------------------------------------------------------------------------------

caminho_livre(Pos1, Pos2, Posicoes, ilha(_, PosI), ilha(_, PosVz)) :-
  posicoes_entre(PosI, PosVz, PosEntre),
  findall(
    Pos,
    (member(Pos, PosEntre), member(Pos, Posicoes)),
    Posicoes_comum
  ),
  length(Posicoes_comum, Len),
  Len \== 1 ; lists:perm([Pos1, Pos2], [PosI, PosVz]).

% ------------------------------------ 2.8 -------------------------------------
% actualiza_vizinhas_entrada(Pos1, Pos2, Posicoes, Entrada, Nova_Entrada)
% actualiza_vizinhas_entrada/5: Apos a adicao de uma ponte entre as ilhas nas
% posicoes Pos1 e Pos2 este predicado atualiza uma entrada, ou seja, remove
% todas as ilhas que deixaram de ser vizinhas devido a essa nova ponte.
% ------------------------------------------------------------------------------

actualiza_vizinhas_entrada(Pos1, Pos2, Posicoes, [I, Vzs, Pontes], [I, Vzs_novo, Pontes]) :-
  findall(
    Vz,
    (member(Vz, Vzs), caminho_livre(Pos1, Pos2, Posicoes, I, Vz)),
    Vzs_aux
  ),
  sort(2, @<, Vzs_aux, Vzs_novo).

% ------------------------------------ 2.9 -------------------------------------
% actualiza_vizinhas_apos_pontes(Estado, Pos1, Pos2, Novo_estado)
% actualiza_vizinhas_apos_pontes/4: Utiliza o predicado
% actualiza_vizinhas_entrada/5 para atualizar todas as entradas de um
% determinado estado.
% ------------------------------------------------------------------------------

actualiza_vizinhas_apos_pontes(Estado, Pos1, Pos2, Novo_estado) :-
  posicoes_entre(Pos1, Pos2, PosEntre),
  maplist(
    actualiza_vizinhas_entrada(Pos1, Pos2, PosEntre),
    Estado, Novo_estado
  ).

% ------------------------------------ 2.10 ------------------------------------
% ilhas_terminadas(Estado, Ilhas_term)
% ilhas_terminadas/2: Percorre todas as entradas de um determinado estado e
% extrai todas as ilhas que estao terminadas. Assim, guarda todas as entradas
% que teem um numero de pontes adequado e diferente de 'X'.
% ------------------------------------------------------------------------------

ilhas_terminadas(Estado, Ilhas_term) :-
  findall(
    Ilha,
    (member([Ilha, _, Pontes], Estado), Ilha = ilha(N_Pontes, _),
    N_Pontes \== 'X', length(Pontes, N_Pontes)),
    Ilhas_term
  ).

% ------------------------------------ 2.11 ------------------------------------
% tira_ilhas_terminadas_entrada(Ilhas_term, Entrada, Nova_Entrada)
% tira_ilhas_terminadas_entrada/3: Retira todas as ilhas terminadas de uma
% entrada, dando origem a uma nova entrada.
% ------------------------------------------------------------------------------

tira_ilhas_terminadas_entrada(Ilhas_term, [I, Vzs, Pontes], [I, Vzs_novo, Pontes]) :-
  subtract(Vzs, Ilhas_term, Vzs_novo).

% ------------------------------------ 2.12 ------------------------------------
% tira_ilhas_terminadas(Estado, Ilhas_term, Novo_estado)
% tira_ilhas_terminadas/3: Remove todas as ilhas terminadas de cada entrada do
% estado atraves do predicado tira_ilhas_terminadas_entrada/3, dando origem a um
% novo estado.
% ------------------------------------------------------------------------------

tira_ilhas_terminadas(Estado, Ilhas_term, Novo_estado) :-
  maplist(
    tira_ilhas_terminadas_entrada(Ilhas_term),
    Estado, Novo_estado
  ).

% ------------------------------------ 2.13 ------------------------------------
% marca_ilhas_terminadas_entrada(Ilhas_term, Entrada, Nova_Entrada)
% marca_ilhas_terminadas_entrada/3: Detecta se a ilha de uma dada entrada esta
% terminada e dah origem a uma nova entrada. Se estiver, substitui o numero de
% pontes por 'X', caso contrario nao altera a entrada.
% ------------------------------------------------------------------------------

marca_ilhas_terminadas_entrada(Ilhas_term, [Ilha, Vzs, Pontes], [Nova_Ilha, Vzs, Pontes]) :-
  Ilha = ilha(_, (N_lin, N_col)),
  member(Ilha, Ilhas_term), Nova_Ilha = ilha('X', (N_lin, N_col)) ;
  Nova_Ilha = Ilha.

% ------------------------------------ 2.14 ------------------------------------
% marca_ilhas_terminadas(Estado, Ilhas_term, Novo_estado)
% marca_ilhas_terminadas/3: Marca todas as ilhas de um estado que se encontram
% terminadas, dando origem a um novo estado. Para esse efeito, recorre ao
% predicado marca_ilhas_terminadas_entrada/3.
% ------------------------------------------------------------------------------

marca_ilhas_terminadas(Estado, Ilhas_term, Novo_estado) :-
  maplist(
    marca_ilhas_terminadas_entrada(Ilhas_term),
    Estado, Novo_estado
  ).

% ------------------------------------ 2.15 ------------------------------------
% trata_ilhas_terminadas(Estado, Novo_estado)
% trata_ilhas_terminadas/2: Retira todas as ilhas terminadas das entradas no
% estado e depois marca todos as entradas com essas ilhas. Deste modo, recorre
% aos predicados tira_ilhas_terminadas/3 e marca_ilhas_terminadas/3.
% ------------------------------------------------------------------------------

trata_ilhas_terminadas(Estado, Novo_estado) :-
  ilhas_terminadas(Estado, Ilhas_term),
  tira_ilhas_terminadas(Estado, Ilhas_term, Estado_aux),
  marca_ilhas_terminadas(Estado_aux, Ilhas_term, Novo_estado).

% ---------------------------------- Auxiliar ----------------------------------
% repete_el(El, N, Lista_rep)
% repete_el/3: Este predicado cria uma lista constituida por N ocorrencias do
% elemento El.
% ------------------------------------------------------------------------------

repete_el(El, 1, [El]) :- !.
repete_el(El, N, [El|R]) :-
  N > 1,
  N_1 is N - 1,
  repete_el(El, N_1, R).

% ---------------------------------- Auxiliar ----------------------------------
% adiciona_pontes(N_Pontes, Ilha1, Ilha2, Entrada, Nova_Entrada)
% adiciona_pontes/5: Adiciona as pontes necessarias ah entrada correspondente a
% essa nova ponte. Utiliza o predicado repete_el/3 para repetir as pontes N
% vezes.
% ------------------------------------------------------------------------------

adiciona_pontes(N_Pontes, Ilha1, Ilha2, [Ilha, Vzs, Pontes_antigas], [Ilha, Vzs, Pontes]) :-
  member(Ilha, [Ilha1, Ilha2]),
  Ilha1 = ilha(_, Pos1), Ilha2 = ilha(_, Pos2),
  cria_ponte(Pos1, Pos2, Ponte_aux),
  repete_el(Ponte_aux, N_Pontes, Pontes_aux),
  append(Pontes_antigas, Pontes_aux, Pontes) ;
  Pontes = Pontes_antigas.

% ------------------------------------ 2.16 ------------------------------------
% junta_pontes(Estado, N_Pontes, Ilha1, Ilha2, Novo_estado)
% junta_pontes/5: Este predicado cria a(s) ponte(s) entre as Ilhas fornecidas
% (Ilha1 e Ilha2), depois adiciona as novas pontes as ilhas correspondentes no
% estado um numero de vezes igual a N_Pontes. Por fim, atualiza todas as
% entradas do estado por meio dos predicados actualiza_vizinhas_apos_pontes/4 e
% trata_ilhas_terminadas/2.
% ------------------------------------------------------------------------------

junta_pontes(Estado, N_Pontes, Ilha1, Ilha2, Novo_estado) :-
  Ilha1 = ilha(_, Pos1), Ilha2 = ilha(_, Pos2),
  maplist(
    adiciona_pontes(N_Pontes, Ilha1, Ilha2),
    Estado, Estado_aux
  ),
  actualiza_vizinhas_apos_pontes(Estado_aux, Pos1, Pos2, Novo_estado_aux),
  trata_ilhas_terminadas(Novo_estado_aux, Novo_estado).

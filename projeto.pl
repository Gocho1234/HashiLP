:- [codigo_comum, puzzles_publicos].

%%% Predicados auxiliares

remove_duplicados([], []).
remove_duplicados([H|T], [H|T1]) :-
  subtract(T, [H], T2), remove_duplicados(T2, T1).

%%% Predicados oficiais

extrai_ilhas_linha(N_linha, Linha, Ilhas) :-
  findall(ilha(El, (N_linha, N_coluna)),
          (member(El, Linha), El \== 0,
          nth1(N_coluna, Linha, El)), Ilhas_aux),
  remove_duplicados(Ilhas_aux, Ilhas).

ilhas(Puz, Ilhas) :-
  findall(Ilha, (member(Linha, Puz), nth1(N_linha, Puz, Linha),
          extrai_ilhas_linha(N_linha, Linha, Ilha)), Ilhas_aux),
  flatten(Ilhas_aux, Ilhas).

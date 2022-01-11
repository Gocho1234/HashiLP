:- [codigo_comum, puzzles_publicos].

extrai_ilhas_linha(N_linha, Linha, Ilhas) :-
  findall(ilha(El, (N_linha, N_coluna)),
          (member(El, Linha), El \== 0,
          nth1(N_coluna, Linha, El)),
          Ilhas).

ilhas(Puz, Ilhas) :-
  findall(Ilha,
          (member(Linha, Puz), nth1(N_linha, Puz, Linha),
          extrai_ilhas_linha(N_linha, Linha, Ilha)),
          Ilhas_aux),
  flatten(Ilhas_aux, Ilhas).

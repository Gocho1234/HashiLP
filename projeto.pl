:- [codigo_comum, puzzles_publicos].

%%% Predicados auxiliares

formata_ilhas(((N_lin, N_col), Ponte), ilha(Ponte, (N_lin, N_col))).

%%% Predicados oficiais

extrai_ilhas_linha(N_lin, Lin, Ilhas) :-
  setof(
    ((N_lin, N_col), El), (member(El, Lin), El \== 0,
    nth1(N_col, Lin, El)), Ilhas_aux
  ),
  maplist(formata_ilhas, Ilhas_aux, Ilhas).


ilhas(Puz, Ilhas) :-
  findall(
    Ilha, (member(Lin, Puz), nth1(N_lin, Puz, Lin),
    extrai_ilhas_linha(N_lin, Lin, Ilha)), Ilhas_aux
  ),
  flatten(Ilhas_aux, Ilhas).

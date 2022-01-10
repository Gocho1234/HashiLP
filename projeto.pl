extrai_ilhas_linha(N_L, Linha, Ilhas) :-
  extrai_ilhas_linha_aux(N_L, Linha, Linha, Ilhas), !.
extrai_ilhas_linha_aux(_, [], _, []).
extrai_ilhas_linha_aux(N_L, [El_1 | Resto_l], Linha, [Ilha_1 | Resto_i]) :-
  El_1 \== 0,
  nth1(N, Linha, El_1),
  Ilha_1 = ilha(El_1, (N_L, N)),
  extrai_ilhas_linha_aux(N_L, Resto_l, Linha, Resto_i).
extrai_ilhas_linha_aux(N_L, [0 | Resto_l], Linha, Resto_i) :-
  extrai_ilhas_linha_aux(N_L, Resto_l, Linha, Resto_i).

generate(M, N, P) :-
    between(4, 19, M),
    between(M, 19, N),
    between(N, 19, P).

generate(M, N, P) :-
    between(5, 19, M),
    between(M, 19, N),
    between(N, 19, P).

config(M,N,P, Q, R) :-
    generate(M, N, P),
    Q is M + N + P,
    R is M * N * P.

config4(M,N,P, Q, R) :-
    generate(M, N, P),
    Q is M - 4 + N - 4 + P - 4,
    R is (M - 4) * (N - 4) * (P - 4).

config41(M,N,P, Q, R) :-
    generate(M, N, P),
    Q is N - 4 + P - 4,
    R is (N - 4) * (P - 4).

all_configs(SFL) :- 
        bagof(([Q, R], [M, N, P]), 
        config(M, N, P, Q, R), L), 
        sort(L, SL), 
        onlydups(SL, DL), 
        flatten(DL, FL), 
        sort(FL, SFL).
all_configs4(SFL) :- 
        bagof(([Q, R], [M, N, P]), 
        config4(M, N, P, Q, R), L), 
        sort(L, SL), 
        onlydups(SL, DL), 
        flatten(DL, FL), 
        sort(FL, SFL).

onlydups([], []).
onlydups([(H,S)|SL], DL) :- onlydups1(SL, H, [S], DL).

onlydups1([(H,S)|SL], H, L, DL) :- 
        !, 
        onlydups1(SL, H, [S|L], DL).
onlydups1(SL, _, [_], DL) :- 
        !, 
        onlydups(SL, DL).
onlydups1(SL, H, L, [(H, L)|DL]) :- onlydups(SL, DL).

flatten([(_,SL)|T], L) :- 
        flatten(T, TL), 
        append(SL, TL, L).
flatten([], []).

solutions(LS) :- 
        all_configs(L), 
        all_configs4(L4), 
        ord_intersect(L, L4, LS).

all_a([]) :- true.
all_a(List) :-
    List = [X | Tail],
    member(X, [a]),
    all_a(Tail).

all_b([]) :- true.
all_b(List) :-
    List = [X | Tail],
    member(X, [b]),
    all_b(Tail).

% turn a list of a's into a list of b's
trans_a_b([], []) :- true.
trans_a_b(List1, List2) :-
    length(List1, L1),
    length(List2, L2),
    L1 =:= L2,
    all_a(List1),
    all_b(List2).


/* examples

% ?- all_a([a, a, a, A, a]).
% ?- all_a([a, a, b, A, a]).
% ?- all_a([a, a, a, A]).
% ?- trans_a_b(L, [b,b]).
% ?- trans_a_b([a, a, a], L).
% ?- trans_a_b([a, a, a], [b]).

*/

is_digit(X) :- between(0, 9, X).
is_digit1(X) :- between(1, 9, X).

% 1935 is represented as [5,3,9,1] 

is_number1([H]) :- is_digit1(H), !.
is_number1([H|T]) :- is_digit(H), is_number1(T).

is_number([0]).
is_number(L) :- is_number1(L).

nat_to_number(0, [0]) :- !.
nat_to_number(N, L) :- nat_to_number1(N, L).

nat_to_number1(0, []).
nat_to_number1(N, [C | R1]) :-
    N > 0,
    divmod(N, 10, N1, C),
    nat_to_number1(N1, R1).

number_to_nat([], 0).
number_to_nat([H|T], N) :-
    number_to_nat(T, N1),
    N is N1 * 10 + H.

% ten(0, 0, 0, 0, 0).
% ten(0, 1, 0, 0, 1).
% ten(0, 2, 0, 0, 2).
% ten(0, 3, 0, 0, 3).
% ten(0, 4, 0, 0, 4).
% ten(0, 5, 0, 0, 5).
% ten(0, 6, 0, 0, 6).

sum(N1, N2, N) :- sum1(0, N1, N2, N).
sum1(0, [], [], []) :- !.
sum1(C, [], N2, N) :- sum1(C, [0], N2, N).
sum1(C, N1, [], N) :- sum1(C, N1, [0], N).
sum1(C, [H1|T1], [H2|T2], [H|T]) :-
    % ten(H1, H2, C, C1, H),
    S is H1 + H2 + C,
    divmod(S, 10, C1, H),
    sum1(C1, T1, T2, T).

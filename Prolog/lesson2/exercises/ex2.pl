% the classic, inefficient method - values are recalculated

fib(0, Y) :- Y is 1.
fib(1, Y) :- Y is 1.
fib(N, Result) :-
    N >= 2,
    N1 is N-1, 
    N2 is N-2, 
    fib(N1, Res1), 
    fib(N2, Res2), 
    Result is Res1 + Res2.

% efficient method

fib2(0, 1).
fib2(1, 1).
fib2(N, X) :- fibAux(N, X, _).

fibAux(1, 1, 1).
fibAux(N, Fn, Fn1) :-
    N >= 2,
    N1 is N-1,
    fibAux(N1, Fn1, Fn2),
    Fn is Fn1 + Fn2.

/* examples

% ?- fib(10, X).
% ?- fib(28, X).
% ?- fib(30, X). - stack limit exceeded
% ?- fib2(50, X).
% ?- fib2(100, X).

*/

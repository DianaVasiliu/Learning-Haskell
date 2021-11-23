line(0, _) :- nl.
line(N, C) :-
    N > 0,
    write(C),
    N1 is N-1,
    line(N1, C).

lines(0, _, _).
lines(M, N, C) :-
  M > 0,
  line(N, C),
  M1 is M-1,
  lines(M1, N, C).

% displaying a square of size N having as element the value C
square(N, C) :- lines(N, N, C).

% displaying a triangle of maximum size N having as element the value C
triangle(N, C) :-
    N > 0,
    line(N, C),
    N1 is N-1,
    triangle(N1, C).

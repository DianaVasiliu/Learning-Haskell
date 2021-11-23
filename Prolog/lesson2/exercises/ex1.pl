distance((X1, Y1), (X2, Y2), Z) :-
    Z is sqrt((X1 - X2) ** 2 + (Y1 - Y2) ** 2).

% `=` makes equality in terms, only replaces variables
% ?- distance((0,0), (3,4), X). 
% will return the result X = sqrt((0-3)**2+(0-4)**2)

% `is` also calculates the expression on the right

% distance((0,0), (3,4), 5).   -> returns false
% distance((0,0), (3,4), 5.0). -> returns true

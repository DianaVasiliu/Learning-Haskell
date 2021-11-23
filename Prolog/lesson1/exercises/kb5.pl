/* GOT riddle 
Varys - "Power is a curious thing, my lord. Are you fond of riddles?" 
Tyrion - "Why? Am I about to hear one?" 
Varys - "Three great men sit in a room, a king, a priest and the rich man. 
         Between them stands a common sellsword. 
         Each great man bids the sellsword kill the other two. 
         Who lives? Who dies?" 
Tyrion - "Depends on the sellsword" 
*/

% Exercise 5

char(king).
char(priest).
char(richMan).

choice(god, priest).
choice(authority, king).
choice(money, richMan).

is_killed(C, X, Y) :-
    char(X), char(Y), \+ choice(C, X), \+ choice(C, Y), X \= Y.


% method 2: (to work for a query ?- is_killed(C, X, Y)):

% we add knowledge about the choices that can be made
% sellsword(god).
% sellsword(authority).
% sellsword(money).

% is_killed(C, X, Y) :-
%     sellsword(C), char(X), char(Y), \+ choice(C, X), \+ choice(C, Y), X \= Y.

/** examples
 * 
?- is_killed(god, X, Y).
?- is_killed(money, X, Y).
?- is_killed(authority, X, Y).
*/
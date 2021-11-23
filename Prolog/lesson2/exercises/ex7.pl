remove_duplicates([], []).
remove_duplicates(List, Result) :-
    List = [Head | Tail],
    member(Head, Tail),
    remove_duplicates(Tail, Result).
remove_duplicates(List, Result) :- 
    List = [Head | Tail],
    \+member(Head, Tail),
    Result = [Head | ResTail],
    remove_duplicates(Tail, ResTail).

% other method

remove_duplicates2([], []).
remove_duplicates2([Head | Tail], Result) :-
    member(Head, Tail),
    remove_duplicates2(Tail, Result).
remove_duplicates2([Head | Tail], [Head | Result]) :-
    not(member(Head, Tail)),
    remove_duplicates2(Tail, Result).

/* examples
 * ?- remove_duplicates([a, b, a, c, d, d], List).
 * ?- remove_duplicates([1,5,6,6,4,2,8,1,1,2], List).
*/

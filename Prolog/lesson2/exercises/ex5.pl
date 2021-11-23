scalarMult(_, [], []).
scalarMult(X, List, Rez) :-
    List = [Elem | Tail],
    Rez = [NewElem | RezTail],
    NewElem is X * Elem,
    scalarMult(X, Tail, RezTail).

dot([], [], 0).
dot(List1, List2, Rez) :- 
    length(List1, L1),
    length(List2, L2),
    L1 =:= L2,
    List1 = [Head1 | Tail1],
    List2 = [Head2 | Tail2],
    P is Head1 * Head2,
    dot(Tail1, Tail2, Rest),
    Rez is P + Rest.

mymax([X], X).
mymax(List, Max) :-
    length(List, L),
    L >= 2,
    List = [Elem | Tail],
    mymax(Tail, Max),
    Max >= Elem.
mymax(List, Elem) :-
    length(List, L),
    L >= 2,
    List = [Elem | Tail],
    mymax(Tail, Max),
    Elem > Max.

max2([X], X).
max2([H | T], Result) :-
    max2(T, Rp),
    Result is max(H, Rp).

max3([Max], Max).
max3([Head | Tail], Max) :- 
    max3(Tail, TailMax),
    Head > TailMax,
    Max = Head.
max3([Head | Tail], Max) :- 
    max3(Tail, TailMax),
    Head =< TailMax,
    Max = TailMax.

/* examples
 * ?- scalarMult(5, [4, 5, 6], Result).
 * ?- scalarMult(3, [2, 7, 4], Result).
 * ?- dot([2, 5, 6], [3, 4, 1], Result).
 * ?- max([4,2,6,8,1], Result).
*/

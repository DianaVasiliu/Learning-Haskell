helperRev([], Reversed, Reversed).
helperRev(List, Rest, Reversed) :-
    List = [Head | Tail],
    helperRev(Tail, [Head | Rest], Reversed).

rev(List, Result) :-
    helperRev(List, [], Result).

palindrome(List) :-
    rev(List, List).

/* examples
?- palindrome([r,e,d,i,v,i,d,e,r]).
?- palindrome([a,b,c,c,b,a]).
?- palindrome([a,b,c,c,b,b]).
?- palindrome([1,a,2,b,b,2,a,1]).
*/

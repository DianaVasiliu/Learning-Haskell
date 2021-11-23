%----------
%  Council
%----------

% Exercise 1

sits_right_of(tyrionlannister, janosslynt).
sits_right_of(janosslynt, cerseibaratheon).
sits_right_of(cerseibaratheon, tywinlannister).
sits_right_of(tywinlannister, petyrbaelish).
sits_right_of(petyrbaelish, varys).
sits_right_of(varys, grandmaesterpycelle).
sits_right_of(grandmaesterpycelle, tyrionlannister).

sits_left_of(X, Y) :- sits_right_of(Y, X).

are_neighbours_of(X, Y, Z) :- 
    sits_left_of(X, Z), sits_right_of(Y, Z).

next_to_each_other(X, Y) :- 
    sits_left_of(X, Y); sits_left_of(Y, X).


/** examples

?- sits_right_of(petyrbaelish, cerseibaratheon).
?- sits_right_of(petyrbaelish, varys).
?- sits_right_of(X, janosslynt).
?- sits_right_of(Y, cerseibaratheon), sits_right_of(X, Y).
?- are_neighbours_of(petyrbaelish, grandmaesterpycelle, X); are_neighbours_of(grandmaesterpycelle, petyrbaelish, X).

*/

%---------------------------------
% Jon Snow and Daenerys Targaryen
%---------------------------------

male(rickardStark).
male(eddardStark).
male(brandonStark).
male(benjenStark).
male(robbStark).
male(branStark).
male(rickonStark).
male(jonSnow).
male(aerysTargaryen).
male(rhaegarTargaryen).
male(viserysTargaryen).
male(aegonTargaryen).

%---------------------------

female(lyarraStark).
female(catelynStark).
female(lyannaStark).
female(sansaStark).
female(aryaStark).
female(rhaellaTargaryen).
female(eliaTargaryen).
female(daenerysTargaryen).
female(rhaenysTargaryen).

%---------------------------

parent_of(rickardStark, eddardStark).
parent_of(rickardStark, brandonStark).
parent_of(rickardStark, benjenStark).
parent_of(rickardStark, lyannaStark).
parent_of(lyarraStark, eddardStark).
parent_of(lyarraStark, brandonStark).
parent_of(lyarraStark, benjenStark).
parent_of(lyarraStark, lyannaStark).

parent_of(eddardStark, robbStark).
parent_of(eddardStark, sansaStark).
parent_of(eddardStark, aryaStark).
parent_of(eddardStark, branStark).
parent_of(eddardStark, rickonStark).
parent_of(catelynStark, robbStark).
parent_of(catelynStark, sansaStark).
parent_of(catelynStark, aryaStark).
parent_of(catelynStark, branStark).
parent_of(catelynStark, rickonStark).

parent_of(aerysTargaryen, rhaegarTargaryen).
parent_of(aerysTargaryen, viserysTargaryen).
parent_of(aerysTargaryen, daenerysTargaryen).

parent_of(rhaellaTargaryen, rhaegarTargaryen).
parent_of(rhaellaTargaryen, viserysTargaryen).
parent_of(rhaellaTargaryen, daenerysTargaryen).

parent_of(rhaegarTargaryen, jonSnow).
parent_of(lyannaStark, jonSnow).

parent_of(rhaegarTargaryen, aegonTargaryen).
parent_of(rhaegarTargaryen, rhaenysTargaryen).

parent_of(eliaTargaryen, aegonTargaryen).
parent_of(eliaTargaryen, rhaenysTargaryen).

% Exercise 2

father_of(Father, Child) :- male(Father), parent_of(Father, Child).

mother_of(Mother, Child) :- female(Mother), parent_of(Mother, Child).

grandfather_of(Grandfather, Child) :- 
    male(Grandfather), father_of(Father, Child), father_of(Grandfather, Father);
    male(Grandfather), mother_of(Mother, Child), father_of(Grandfather, Mother).
    
grandmother_of(Grandmother, Child) :- 
    female(Grandmother), mother_of(Mother, Child), mother_of(Grandmother, Mother);
    female(Grandmother), father_of(Father, Child), mother_of(Grandmother, Father).

sister_of(Sister, Person) :-
    female(Sister), parent_of(X, Sister), parent_of(X, Person), Sister \= Person.

brother_of(Brother, Person) :-
    male(Brother), parent_of(X, Brother), parent_of(X, Person), Brother \= Person.

aunt_of(Aunt, Person) :-
    female(Aunt), parent_of(Parent, Person), sister_of(Aunt, Parent).

uncle_of(Uncle, Person) :- 
	male(Uncle), parent_of(Parent, Person), brother_of(Uncle, Parent).

/** examples

?- aunt_of(daenerysTargaryen, jonSnow).
?- parent_of(X, sansaStark).
?- father_of(rhaegarTargaryen, X).
?- grandmother_of(X, aryaStark).
?- grandmother_of(lyannaStark, aryaStark).
?- sister_of(aryaStark, X).

*/

% Exercise 3

ancestor_of(Ancestor, Person) :-
    parent_of(Ancestor, Person);
    parent_of(X, Person), ancestor_of(Ancestor, X).

/** examples
?- ancestor_of(rickardStark, jonSnow).
*/

% Exercise 4

not_parent(X, Y) :- 
    female(X), \+ parent_of(X, Y);
    male(X), \+ parent_of(X, Y).
    
ancestor_not_parent(X, Y) :-
    ancestor_of(X, Y),
    not_parent(X, Y).

/** examples
?- not_parent(X, Y) :- not(parent_of(X,Y)).
?- not_parent(X, Y) :- \+ parent_of(X,Y).

?- not_parent(rickardStark, viserysTargaryen).  % -> true
?- not_parent(X, jonSnow).  % -> false
?- not_parent(X, Y).  % -> false
*/



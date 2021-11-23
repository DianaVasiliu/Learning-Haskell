% verifying the syntax of the IMP language

/*
 * Operator definition
    https://www.swi-prolog.org/pldoc/man?section=operators

 * Expression
E ::= 
    n
	|x
    |E + E
    |E - E
    |E * E
    |E / E
    |E // E
    |E ** E
    |E @ E
    |- E
    |++E
    |E++
    |--E
    |E--

 * Boolean
B ::= 
    true
	|false
    |E < E
    |E =< E
    |E > E
    |E >= E
    |E == E
    |E /= E
    |not(B)
    |and(B, B)
    |or(B, B)
    |xor(B, B)

 * Instructions
C ::= 
    skip
	|x = E;
    |C; C;
    |(C; C)
    |{C}
    |if(B) C else C
    |while(B) C
    |for(C, B, C) C
    |dowhile(C, B)

 * Programs
P ::= {C}, E

*/

:- op(100, xf, {}).
:- op(1100, yf, ;).
:- op(400, yfx, @).  % modulo
:- op(700, xfx, /=).
:- op(200, xfx, **).
:- op(200, fy, -).
:- op(200, fx, ++).
:- op(200, xf, ++).
:- op(200, fx, --).
:- op(200, xf, --).

aexp(I) :- integer(I).
aexp(X) :- atom(X).
aexp(A1 + A2) :- aexp(A1), aexp(A2).
aexp(A1 - A2) :- aexp(A1), aexp(A2).
aexp(A1 * A2) :- aexp(A1), aexp(A2).
aexp(A1 / A2) :- aexp(A1), aexp(A2).
aexp(A1 // A2) :- aexp(A1), aexp(A2).
aexp(A1 ** A2) :- aexp(A1), aexp(A2).
aexp(A1 @ A2) :- aexp(A1), aexp(A2).  % modulo
aexp(- A1) :- aexp(A1).
aexp(++ A1) :- aexp(A1).
aexp(A1 ++) :- aexp(A1).
aexp(-- A1) :- aexp(A1).
aexp(A1 --) :- aexp(A1).

bexp(true).
bexp(false).
bexp(not(BE)) :- bexp(BE).
bexp(and(BE1, BE2)) :- bexp(BE1), bexp(BE2).
bexp(or(BE1, BE2)) :- bexp(BE1), bexp(BE2).
bexp(xor(BE1, BE2)) :- bexp(BE1), bexp(BE2).
bexp(A1 < A2) :- aexp(A1), aexp(A2).
bexp(A1 =< A2) :- aexp(A1), aexp(A2).
bexp(A1 > A2) :- aexp(A1), aexp(A2).
bexp(A1 >= A2) :- aexp(A1), aexp(A2).
bexp(A1 == A2) :- aexp(A1), aexp(A2).
bexp(A1 /= A2) :- aexp(A1), aexp(A2).

stmt(skip).
stmt(X = AE) :- atom(X), aexp(AE).
stmt(St1; St2;) :- stmt(St1), stmt(St2).
stmt((St1; St2)) :- stmt(St1), stmt(St2).
stmt({St}) :- stmt(St).
stmt(if(BE, St1, St2)) :- bexp(BE), stmt(St1), stmt(St2).
stmt(while(BE, St)) :- bexp(BE), stmt(St).
stmt(for(St1, BE, St2, St)) :- stmt(St1), bexp(BE), stmt(St2), stmt(St).
stmt(dowhile(St, BE)) :- stmt(St), bexp(BE).

program(St, AE) :- stmt(St), aexp(AE).

test0 :- program( 
            {
            x = 10;
            sum = 0;
            while(x >= 0,
                    {
                        sum = sum + x;
                        x = x - 1;
                    }
            );
            for(i = 0, i =< 10, i = i + 1,
                {
                    x = i;
                    if(xor(x @ 2 /= 0, x @ 3 == 0),
                	   sum = sum ** 2,
                       sum = x * 2 // 3; sum = -sum)
                }
            );
            sum = -sum;
            sum = ++ sum;
            sum = sum --;
            }, 
            sum
        ).

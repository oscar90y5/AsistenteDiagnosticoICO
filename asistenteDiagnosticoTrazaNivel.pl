:-op(40, xfy, &).
:-op(50, xfy, --->).

solve(true):-!.
solve((A & B)) :-!, solve(A), solve(B).
solve(A) :- !, B ---> A, solve(B).

solve_traza(true):-!.
solve_traza((A & B)) :-!, solve_traza(A,T1), solve_traza(B,T1).
solve_traza(A):-
	write('Call: '), write(A), nl,
	B ---> A, solve_traza(B),
	write('Exit: '), write(A), nl.

solve_traza_nivel(A):-solve_traza_nivel(A,0).
solve_traza_nivel(true,_):-!.
solve_traza_nivel((A & B),T) :-!, T1 is T + 1, solve_traza_nivel(A,T1), solve_traza_nivel(B,T1).
solve_traza_nivel(A,T):-
	tab(4*T),write('Call '),write(T),write(':'), write(A), nl,
	B ---> A, solve_traza_nivel(B,T),
	tab(4*T),write('Exit '),write(T),write(':'), write(A), nl.

true ---> live(outside).

true ---> light(l1).
true ---> light(l2).

true ---> down(s1).
true ---> up(s2).
true ---> up(s3).

true ---> ok(_).

true ---> connected_to(w5,outside).
true ---> connected_to(p2,w6).
true ---> connected_to(p1,w3).
true ---> connected_to(l2,w4).
true ---> connected_to(l1,w0).

up(s2) & ok(s2) ---> connected_to(w0,w1).
down(s2) & ok(s2) ---> connected_to(w0,w2).

up(s1) & ok(s1) ---> connected_to(w1,w3).
down(s1) & ok(s1) ---> connected_to(w2,w3).

up(s3) & ok(s3) ---> connected_to(w4,w3).

ok(cb1) ---> connected_to(w3,w5).
ok(cb2) ---> connected_to(w6,w5).


light(L) & ok(L) & live(L) ---> lit(L).
connected_to(W,V) & live(V) ---> live(W).



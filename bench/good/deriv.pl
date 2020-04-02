:- style_check(-singleton).
:- use_module(type_check).
:- style_check(+singleton).
 :- ensure_loaded(harness).

:- type empty.
:- type quad(T0,T1,T2,T3) ---> quad(T0,T1,T2,T3).
:- type expr_var ---> x ; y ; z.
:- type expr ---> i(integer) ; v(expr_var) ; expr + expr ; expr - expr ; expr * expr ; expr / expr ; ^(expr, integer) ; -expr ; exp(expr) ; log(expr).
:- type quad_expr ---> quad(expr, expr, expr, expr).

:- pred expr_var(expr_var).
expr_var(x).
expr_var(y).
expr_var(z).

:- pred data(empty).
data(_Data).

:- pred benchmark(empty, quad_expr).
benchmark(_Data, quad(E1, E2, E3, E4)) :-
	ops8(E1), divide10(E2), log10(E3), times10(E4).

:- pred ops8(expr).
ops8(E) :-
	d((v(x) + i(1)) * ((^(v(x), 2) + i(2)) * (^(v(x), 3) + i(3))), x, E).

:- pred divide10(expr).
divide10(E) :-
	d(((((((((v(x) / v(x)) / v(x)) / v(x)) / v(x)) / v(x)) / v(x)) / v(x)) / v(x)) / v(x), x, E).

:- pred log10(expr).
log10(E) :-
	d(log(log(log(log(log(log(log(log(log(log(v(x))))))))))), x, E).

:- pred times10(expr).
times10(E) :-
	d(((((((((v(x) * v(x)) * v(x)) * v(x)) * v(x)) * v(x)) * v(x)) * v(x)) * v(x)) * v(x), x, E).

:- pred d(expr, expr_var, expr).
d(U + V, X, DU + DV) :-
	!,
	d(U, X, DU),
	d(V, X, DV).
d(U - V, X, DU - DV) :-
	!,
	d(U, X, DU),
	d(V, X, DV).
d(U * V, X, DU * V + U * DV) :-
	!,
	d(U, X, DU),
	d(V, X, DV).
d(U / V, X, (DU * V - U * DV) / ^(V, 2)) :-
	!,
	d(U, X, DU),
	d(V, X, DV).
d(^(U, N), X, DU * i(N) * ^(U, N1)) :-
	!,
	N1 is N - 1,
	d(U, X, DU).
d(-U, X, -DU) :-
	!,
	d(U, X, DU).
d(exp(U), X, exp(U) * DU) :-
	!,
	d(U, X, DU).
d(log(U), X, DU / U) :-
	!,
	d(U, X, DU).
d(v(X), X, i(1)) :- !, expr_var(X).
d(v(X), Y, i(0)) :- X \== Y, !, expr_var(X), expr_var(Y).
d(i(N), X, i(0)) :- integer(N), expr_var(X).


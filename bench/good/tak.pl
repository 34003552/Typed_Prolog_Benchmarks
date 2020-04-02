
:- style_check(-singleton).
:- use_module(type_check).
:- style_check(+singleton).

:- ensure_loaded(harness).

:- type triple(T0, T1, T2) ---> triple(T0, T1, T2).
:- type triple_integer ---> triple(integer, integer, integer).

:- pred data(triple_integer).
data(triple(18, 12, 6)).

:- pred benchmark(triple_integer, integer).
benchmark(triple(X, Y, Z), Out) :-
	tak(X, Y, Z, Out).

:- pred tak(integer, integer, integer, integer).
tak(X,Y,Z,A) :-
	X =< Y, !,
	Z = A.
tak(X,Y,Z,A) :-
	% X > Y,
	X1 is X - 1,
	tak(X1,Y,Z,A1),
	Y1 is Y - 1,
	tak(Y1,Z,X,A2),
	Z1 is Z - 1,
	tak(Z1,X,Y,A3),
	tak(A1,A2,A3,A).

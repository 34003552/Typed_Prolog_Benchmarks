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
	call(tak(X, Y, Z, Out)).

:- pred tak(integer, integer, integer, integer).
tak(X,Y,Z,A) :-
	call(X =< Y), !,
	call(Z = A).
tak(X,Y,Z,A) :-
	% X > Y,
	call(X1 is X - 1),
	call(tak(X1,Y,Z,A1)),
	call(Y1 is Y - 1),
	call(tak(Y1,Z,X,A2)),
	call(Z1 is Z - 1),
	call(tak(Z1,X,Y,A3)),
	call(tak(A1,A2,A3,A)).

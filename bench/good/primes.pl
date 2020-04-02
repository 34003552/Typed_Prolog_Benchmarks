%	pri2

:- style_check(-singleton).
:- use_module(type_check).
:- style_check(+singleton).

:- ensure_loaded(harness).

:- pred benchmark(integer, list(integer)).
benchmark(Data, Out) :-
	primes(Data, Out).

:- pred data(integer).
data(98).

:- pred primes(integer, list(integer)).
primes(Limit, Ps) :-
	integers(2, Limit, Is),
	sift(Is, Ps).

:- pred integers(integer, integer, list(integer)).
integers(Low, High, [Low | Rest]) :- 
	Low =< High, !,
	M is Low + 1,
	integers(M, High, Rest).
integers(_,_,[]).

:- pred sift(list(integer), list(integer)).
sift([], []).
sift([I | Is], [I | Ps]) :-
	remove(I, Is, New),
	sift(New, Ps).

:- pred remove(integer, list(integer), list(integer)).
remove(_P,[],[]).
remove(P,[I | Is], Nis0) :-
	I mod P =\= 0, !,
	Nis0 = [I | Nis],
	remove(P,Is,Nis).
remove(P,[_I | Is], Nis) :-
	remove(P,Is,Nis).

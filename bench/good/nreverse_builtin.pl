%   nreverse
%
%   David H. D. Warren
%
%   "naive"-reverse a list of 30 integers

:- style_check(-singleton).
:- use_module(type_check).
:- style_check(+singleton).

:- ensure_loaded(harness).

:- trust_pred append(list(integer), list(integer), list(integer)).

:- pred benchmark(list(integer), list(integer)).
benchmark(Data, Out) :-
	nreverse(Data, Out).

:- pred data(list(integer)).
data([1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,
	16,17,18,19,20,21,22,23, 24,25,26,27,28,29,30]).

:- pred nreverse(list(integer), list(integer)).
nreverse([], []).
nreverse([X|L0], L) :-
	nreverse(L0, L1), append(L1, [X], L).

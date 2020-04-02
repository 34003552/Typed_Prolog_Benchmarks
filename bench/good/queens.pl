% 9-queens program

:- style_check(-singleton).
:- use_module(type_check).
:- style_check(+singleton).

:- ensure_loaded(harness).

:- pred benchmark(list(integer), list(integer)).
benchmark(Data, Out) :-
	queen(Data, Out).

:- pred data(list(integer)).
data([1,2,3,4,5,6,7,8,9]).

% #if	defined(CONSTRAINT_PROPAGATION)

:- pred queen(list(integer), list(integer)).
queen(Data, Out) :-
	queen_2(Data, [], Out).

:- pred queen_2(list(integer), list(integer), list(integer)).
queen_2([], _, []).
queen_2([H|T], History, [Q|M]) :-
	qdelete(Q, H, T, L1),
	nodiag(History, Q, 1),
	queen_2(L1, [Q|History], M).

% #elif	defined(COROUTINING)
% 
% queen(Data, Out) :-
% 	safe(Out),
% 	qperm(Data, Out).
% 
% #else
% 
% queen(Data, Out) :-
% 	qperm(Data, Out),
% 	safe(Out).
% 
% #endif

:- pred qperm(list(integer), list(integer)).
qperm([], []).
qperm([X|Y], [U|V]) :-
	qdelete(U, X, Y, Z),
	qperm(Z, V).

:- pred qdelete(integer, integer, list(integer), list(integer)).
qdelete(A, A, L, L).
qdelete(X, A, [H|T], [A|R]) :-
	qdelete(X, H, T, R).

:- pred safe(list(integer)).
safe([]).
safe([N|L]) :-
	nodiag(L, N, 1),
	safe(L).

:- pred nodiag(list(integer), integer, integer).
nodiag([], _, _).
nodiag([N|L], B, D) :-
	D =\= N - B,
	D =\= B - N,
	D1 is D + 1,
	nodiag(L, B, D1).

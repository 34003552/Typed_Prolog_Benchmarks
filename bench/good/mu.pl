% generated: 9 November 1989
% option(s): 
%
%   mu
%
%   derived from Douglas R. Hofstadter, "Godel, Escher, Bach," pages 33-35.
%
%   prove "mu-math" theorem muiiu

:- style_check(-singleton).
:- use_module(type_check).
:- style_check(+singleton).

:- ensure_loaded(harness).

:- type mu_t ---> i ; m ; u ; a ; to_mu_t(integer).

:- trust_pred length(list(mu_t), integer).

:- pred benchmark(list(mu_t), list(list(mu_t))).
benchmark(Data, Out) :-
	length(Data, Len),
	theorem(Data, Len, Out).

:- pred data(list(mu_t)).
data([m,u,i,i,u]).

:- pred theorem(list(mu_t), integer, list(list(mu_t))).
theorem([m,i], _, [[a|[m,i]]]).
theorem(R, Depth, [[to_mu_t(N)|R]|P]) :-
    Depth > 0,
    D is Depth-1,
    theorem(S, D, P),
    rule(N, S, R).

:- pred rule(integer, list(mu_t), list(mu_t)).
rule(1, S, R) :- rule1(S, R).
rule(2, S, R) :- rule2(S, R).
rule(3, S, R) :- rule3(S, R).
rule(4, S, R) :- rule4(S, R).

:- pred rule1(list(mu_t), list(mu_t)).
rule1([i], [i,u]).
rule1([H|X], [H|Y]) :-
    rule1(X, Y).

:- pred rule2(list(mu_t), list(mu_t)).
rule2([m|X], [m|Y]) :- 
    concatenate(X, X, Y).

:- pred rule3(list(mu_t), list(mu_t)).
rule3([i,i,i|X], [u|X]).
rule3([H|X], [H|Y]) :-
    rule3(X, Y).

:- pred rule4(list(mu_t), list(mu_t)).
rule4([u,u|X], X).
rule4([H|X], [H|Y]) :-
    rule4(X, Y).

:- pred concatenate(list(mu_t), list(mu_t), list(mu_t)).
concatenate([], X, X).
concatenate([A|B], X, [A|B1]) :-
    concatenate(B, X, B1).

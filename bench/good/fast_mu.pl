%
%	The MU-puzzle
%		from Hofstadter's "Godel, Escher, Bach" (pp. 33-6).
%		written by Bruce Holmer
%
%	To find a derivation type, for example: 
%		theorem([m,u,i,i,u]).
%	Also try 'miiiii' (uses all rules) and 'muui' (requires 11 steps).
%	Note that it can be shown that (# of i's) cannot be a multiple
%	of three (which includes 0).
%	Some results:
%
%	string		# steps
%	------		-------
%	miui		8
%	muii		8
%	muui		11
%	muiiu		6
%	miuuu		9
%	muiuu		9
%	muuiu		9
%	muuui		9

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- style_check(-singleton).
:- use_module(type_check).
:- style_check(+singleton).

:- ensure_loaded(harness).

:- type empty.
:- type imu ---> i ; m ; u.
:- type rule ---> rule(integer, list(imu)).

:- trust_pred length(list(imu), integer).

:- pred benchmark(list(imu), empty).
benchmark(Data, _Out) :-
	theorem(Data).

:- pred data(list(imu)).
data([m,u,i,i,u]).

% First break goal atom into a list of characters,
% find the derivation
:- pred theorem(list(imu)).
theorem(G) :-
	length(G, GL1),
	GL is GL1 - 1,
	derive([m,i], G, 1, GL, _Derivation, 0).

% derive(StartString, GoalString, StartStringLength, GoalStringLength,
%		Derivation, InitBound).
:- pred derive(list(imu), list(imu), integer, integer, list(rule), integer).
derive(S, G, SL, GL, D, B) :- 
	derive2(S, G, SL, GL, 1, D, B).
derive(S, G, SL, GL, D, B) :- 
	B1 is B + 1,
	derive(S, G, SL, GL, D, B1).

% derive2(StartString, GoalString, StartStringLength, GoalStringLength,
%		ScanPointer, Derivation, NumRemainingSteps).
:- pred derive2(list(imu), list(imu), integer, integer, integer, list(rule), integer).
derive2(S, S, SL, SL, _,   [], _).
derive2(S, G, SL, GL, Pin, [rule(N,I)|D], R) :-
	lower_bound(SL, GL, B),
	R >= B,
	R1 is R - 1,
	rule(S, I, SL, IL, Pin, Pout, N),
	derive2(I, G, IL, GL, Pout, D, R1).

:- pred rule(list(imu), list(imu), integer, integer, integer, integer, integer).
rule([m|T1], [m|T2], L1, L2, Pin, Pout, N) :-
	rule(T1, T2, L1, L2, Pin, Pout, 1, i, N, X, X).

% rule(InitialString, FinalString, InitStrLength, FinStrLength,
%		ScanPtrIn, ScanPtrOut, StrPosition, PreviousChar,
%		RuleNumber, DiffList, DiffLink).
%   The difference list is used for doing a list concatenate in rule 2.
:- pred rule(list(imu), list(imu), integer, integer, integer, integer, integer, imu, integer, list(imu), list(imu)).
rule([],        L,      L1, L2, _,   1,    _,   _, 2, L, []) :-
							L2 is L1 + L1.
rule([i],       [i,u],  L1, L2, Pin, Pout, Pos, _, 1, _, _) :- 
							Pos >= Pin,
							Pout is Pos - 2,
							L2 is L1 + 1.
rule([i,i,i|T], [u|T],  L1, L2, Pin, Pout, Pos, _, 3, _, _) :- 
							Pos >= Pin,
							Pout is Pos - 1,
							L2 is L1 - 2.
rule([u,u|T],   T,      L1, L2, Pin, Pout, Pos, i, 4, _, _) :-
							Pos >= Pin,
							Pout is Pos - 2,
							L2 is L1 - 2.
rule([H|T1],    [H|T2], L1, L2, Pin, Pout, Pos, _, N, L, [H|X]) :-
	Pos1 is Pos + 1,
	rule(T1,  T2,  L1, L2, Pin, Pout, Pos1, H, N, L, X).

:- pred lower_bound(integer, integer, integer).
lower_bound(N, M, 1) :- N < M.
lower_bound(N, N, 2).
lower_bound(N, M, B) :-
        N > M,
        Diff is N - M,
	P is Diff/\1,             % use and to do even test
        (P =:= 0 ->
                B is Diff >> 1;   % use shifts to divide by 2
                B is ((Diff + 1) >> 1) + 1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Copyright (C) 1990 Peter Van Roy and Regents of the University of California.
% All rights reserved.  This program may be freely used and modified for
% non-commercial purposes provided this copyright notice is kept unchanged.
% Written by Peter Van Roy as a part of the Aquarius project.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Benchmark based on the Aquarius compiler flow analyzer version 1.
% This program does a dataflow analysis of quicksort using abstract
% interpretation.  The lattice has two useful values: uninit and ground.
% Analysis takes three passes (it prints three 'x' characters).
% Builtins used: compare/3, arg/3, functor/3, sort/2, keysort/2, ==/2, \==/2.

:- style_check(-singleton).
:- use_module(type_check).
:- style_check(+singleton).

:- ensure_loaded(harness).

:- style_check(-singleton).

:- type empty.
:- type yn_t ---> 'yes' ; 'no'.
:- type state ---> 'unknown' ; 'any' ; 'uninit' ; 'ground'.
:- type clause ---> (any :- any) ; (any :- node(integer, beta_t, node, node)).
:- type stree ---> stree(any, clause, any, any, any).
:- type node(T0, T1, T2, T3) ---> node(T0, T1, T2, T3) ; 'leaf'.
:- type node ---> 'leaf' ; to_node(node(integer, beta_t, node, node)).
:- type functor_t ---> node(integer, beta_t, node, node) ; (functor_t = functor_t) ; to_functor_t(any) ; to_functor_t(beta_t).
:- type alpha_t ---> 'true' ; 'fail' ; (functor_t, alpha_t) ; to_alpha_t((any ; any)) ; to_alpha_t(functor_t).
:- type gamma_t ---> any/integer ; to_gamma_t(integer).
:- type accessor ---> get(integer, functor_t) ; set(integer, functor_t) ; get(gamma_t, functor_t) ; set(gamma_t, functor_t).
:- type beta_t ---> to_beta_t(functor_t).
:- type delta_t ---> to_delta_t(functor_t).
:- type epsilon_t ---> (ground(any),any) ; (uninit(any),any) ; true.
:- type zeta_t.
:- type eta_t ---> add_set(list(delta_t)) ; sub_set(list(delta_t)) ; add(delta_t) ; sub(delta_t).

:- type stree_t ---> stree(gamma_t, any, any, list(stree_t), integer).

%:- trust_pred filter_vars(any, any, any, any).
%:- trust_pred sort(list(any), list(any)).
:- trust_pred keysort(list(any), list(any)).
%:- trust_pred filter_dups(any, any, any).
:- trust_pred \+(any).

:- pred data(empty).
data(_).

:- pred benchmark(empty, node(integer,beta_t,node,node)).
benchmark(_Data, Table) :-
	analyze_strees(
	  [stree(main/0,
	      (main:-
	       (qsort([1,2],_,[]),true
               ;fail
	       )),
              (main:-true),[],1),
	   stree(qsort/3,
	      (qsort(U,P,Q):-
	       (U=[N|O],part(O,N,R,S),qsort(S,T,Q),qsort(R,P,[N|T]),true
	       ;U=[],Q=P,true
	       ;fail
	       )),
	      (qsort(_,_,_):-true),[],1),
	   stree(part/4,
	      (part(W,X,Y,Z):-
	       ('$cut_load'(A1),'$cut_part/4_1'(W,X,Y,Z,A1),true
	       ;fail
	       )),
	      (part(_,_,_,_):-true),
	   [stree('$cut_part/4_1'/5,
	      ('$cut_part/4_1'(I1,E1,F1,G1,H1):-
	       (I1=[C1|D1],'$fac_$cut_part/4_1/5_2'(D1,E1,F1,G1,H1,C1),true
	       ;I1=[],F1=[],G1=[],true
	       ;fail
	       )),
	      ('$cut_part/4_1'(_,_,_,_,_):-true),
	    [stree('$fac_$cut_part/4_1/5_2'/6,
	      ('$fac_$cut_part/4_1/5_2'(K1,L1,Q1,O1,P1,M1):-
	       (Q1=[M1|N1],M1=<L1,'$cut_shallow'(P1),part(K1,L1,N1,O1),true
	       ;O1=[M1|R1],part(K1,L1,Q1,R1),true
	       ;fail
	       )),
	      ('$fac_$cut_part/4_1/5_2'(_,_,_,_,_,_):-true),[],1)
	    ],1)
	   ],1)
	  ], Table).

:- pred analyze_strees(list(stree_t), node(integer,beta_t,node,node)).
analyze_strees(Strees, OutTable) :-
	init_strees(Strees, _, Table),
	seal(Table),
	analyze_closure(Strees, Table, OutTable).

% Repeat traversal step until there are no more changes:
:- pred analyze_closure(list(stree_t), node(integer,beta_t,node,node), node(integer,beta_t,node,node)).
analyze_closure(Strees, InTable, OutTable) :-
	traverse_strees(Strees, InTable, MidTable, 0, Changes),
	% Mark an analysis pass:
	% put("x"), nl,
	analyze_closure(Strees, MidTable, OutTable, Changes).

:- pred analyze_closure(list(stree_t), node(integer,beta_t,node,node), node(integer,beta_t,node,node), integer).
analyze_closure(_Strees, InTable, InTable, N) :- N=<0, !.
analyze_closure(Strees, InTable, OutTable, N) :- N>0, !,
	analyze_closure(Strees, InTable, OutTable).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Initialize the table of call lattice values:

:- pred init_strees(list(stree_t), node(integer,beta_t,node,node), node(integer,beta_t,node,node)).
init_strees([],_4,_4) :-
   true.
init_strees([_12|_13],_4,_5) :-
   _12=stree(_14,(_15:-_16),_17,_18,_19),
   bottom_call(_14,_20),
   table_command(get(_14,_20),_4,_23),
   init_disj(_16,_23,_24),
   init_strees(_18,_24,_25),
   init_strees(_13,_25,_5),
   true.

:- pred init_conj(alpha_t, node(integer,beta_t,node,node), node(integer,beta_t,node,node)).
init_conj(true,_4,_4) :-
   true.
init_conj((_12,_13),_4,_5) :-
   init_goal(_12,_4,_16),
   init_conj(_13,_16,_5),
   true.

:- pred init_disj(alpha_t, node(integer,beta_t,node,node), node(integer,beta_t,node,node)).
init_disj(fail,_4,_4) :-
   true.
init_disj(to_alpha_t(_12;_13),_4,_5) :-
   init_conj(_12,_4,_16),
   init_disj(_13,_16,_5),
   true.

:- pred init_goal(functor_t, node(integer,beta_t,node,node), node(integer,beta_t,node,node)).
init_goal(_3,_4,_5) :-
   call_p(_3),
   !,
   functor(_3,_12,_13),
   bottom_call(_12/_13,_14),
   table_command(get(_12/_13,_14),_4,_5),
   true.
init_goal(_3,_4,_4) :-
   unify_p(_3),
   !,
   true.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- pred traverse_strees(list(stree_t), node(integer,beta_t,node,node), node(integer,beta_t,node,node), integer, integer).
traverse_strees([],_4,_4,_6,_6) :-
   true.
traverse_strees([_14|_15],_4,_5,_6,_7) :-
   _14=stree(_16,(_17:-_18),_19,_20,_21),
   traverse_disj(_17,_18,_4,_26,_6,_27),
   traverse_strees(_20,_26,_28,_27,_29),
   traverse_strees(_15,_28,_5,_29,_7),
   true.

:- pred traverse_disj(functor_t, alpha_t, node(integer,beta_t,node,node), node(integer,beta_t,node,node), integer, integer).
traverse_disj(_3,fail,_5,_5,_7,_7) :-
   true.
traverse_disj(_3,to_alpha_t((_15;_16)),_5,_6,_7,_8) :-
   traverse_conj(_3,_15,_5,_22,_7,_23),
   traverse_disj(_3,_16,_22,_6,_23,_8),
   true.

:- pred traverse_conj(functor_t, node(integer,beta_t,node,node), node(integer,beta_t,node,node), node(integer,beta_t,node,node), integer, integer).
traverse_conj(_3,_4,_5,_6,_7,_8) :-
   varset(_3,_24),
   functor(_3,_15,_16),
   table_command(get(_15/_16,_17),_5,_25),
   get_entry_modes(uninit,_3,_17,_26),
   get_entry_modes(ground,_3,_17,_27),
   traverse_conj(to_alpha_t(_4),_25,_6,_7,_8,_27,_28,_26,_29,_24,_30),
   true.

:- pred traverse_conj(alpha_t, node(integer,beta_t,node,node), node(integer,beta_t,node,node), integer, integer, list(delta_t), list(delta_t), list(delta_t), list(delta_t), list(delta_t), list(delta_t)).
traverse_conj(true,_4,_4,_6,_6,_8,_8,_10,_10,_12,_12) :-
   true.
traverse_conj((_20,_21),_4,_5,_6,_7,_8,_9,_10,_11,_12,_13) :-
   varset(_20,_32),
   update_goal(_20,_32,_4,_33,_6,_34,_8,_35,_10,_36,_12,_37),
   unionv(_32,_37,_38),
   traverse_conj(_21,_33,_5,_34,_7,_35,_9,_36,_11,_38,_13),
   true.

:- pred update_goal(functor_t, list(delta_t), node(integer,beta_t,node,node), node(integer,beta_t,node,node), integer, integer, list(delta_t), list(delta_t), list(delta_t), list(delta_t), list(delta_t), list(delta_t)).
update_goal(_3,_4,_5,_5,_7,_7,_9,_10,_11,_12,_13,_13) :-
   split_unify(_3,_21,_27),
   var(_21),
   nonvar(_27),
   varset(_27,_28),
   subsetv(_28,_9),
   !,
   set_command(add(to_delta_t(_21)),_9,_10),
   set_command(sub(to_delta_t(_21)),_11,_12),
   true.
update_goal(_3,_4,_5,_5,_7,_7,_9,_9,_11,_12,_13,_13) :-
   split_unify(_3,to_functor_t(_21),_30),
   var(_21),
   nonvar(_30),
   inv(_21,_11),
   !,
   diffv(_4,_13,_31),
   diffv(_31,_9,_22),
   set_command(add_set(_22),_11,_32),
   set_command(sub(_21),_32,_33),
   intersectv(_4,_13,_23),
   set_command(sub_set(_23),_33,_12),
   true.
update_goal(_3,_4,_5,_5,_7,_7,_9,_10,_11,_12,_13,_13) :-
   split_unify(_3,to_functor_t(_27),_28),
   var(_27),
   inv(_27,_9),
   !,
   set_command(add_set(_4),_9,_10),
   set_command(sub_set(_4),_11,_12),
   true.
update_goal(_3,_4,_5,_5,_7,_7,_9,_9,_11,_12,_13,_13) :-
   unify_p(_3),
   !,
   set_command(sub_set(_4),_11,_12),
   true.
update_goal(_3,_4,_5,_6,_7,_8,_9,_9,_11,_12,_13,_13) :-
   call_p(_3),
   !,
   goal_dupset(_3,_33),
   var_args(_3,_34),
   functor(_3,_22,_23),
   functor(_35,_22,_23),
   create_new_call(1,_23,_9,_34,_33,_11,_13,_3,_35),
   update_table(_22/_23,_35,_5,_6,_7,_8),
   set_command(sub_set(_4),_11,_12),
   true.

:- pred update_table(any, functor_t, node(integer,beta_t,node,node), node(integer,beta_t,node,node), integer, integer).
update_table(_15/_16,_4,_5,_6,_7,_8) :-
   table_command(get(_15/_16,_18),_5,_24),
   lub_call(_18,_4,_19),
   _18\==_19,
   !,
   table_command(set(_15/_16,_19),_24,_6),
   _8 is _7+1,
   true.
update_table(_15/_16,_4,_5,_5,_7,_7).

:- pred create_new_call(integer, integer, list(delta_t), list(delta_t), list(delta_t), list(delta_t), list(delta_t), functor_t, functor_t).
create_new_call(I, Ar, _, _, _, _, _, _, _) :- I>Ar, !.
create_new_call(I, Ar, Gnd, VarArgs, DupVars, Uni, SoFar, Goal, Call) :-
	I=<Ar,
	!,
	arg(I, Goal, X),
	arg(I, Call, Y),
	ground_flag(X, Gnd, Gf),
	membership_flag(X, VarArgs, Vf),
	membership_flag(X, DupVars, Df),
	membership_flag(X, Uni, Uf),
	membership_flag(X, SoFar, Sf),
	create_argument(Gf, Vf, Df, Uf, Sf, Y),
	I1 is I+1,
	create_new_call(I1, Ar, Gnd, VarArgs, DupVars, Uni, SoFar, Goal, Call).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Lattice utilities:


:- pred lub(state, state, state).
lub(unknown,       X,      X) :- !.
lub(      X, unknown,      X) :- !.
lub(    any,       _,    any) :- !.
lub(      _,     any,    any) :- !.
lub( uninit,  uninit, uninit) :- !.
lub( ground,  ground, ground) :- !.
lub( uninit,  ground,    any) :- !.
lub( ground,  uninit,    any) :- !.

:- pred bottom(state).
bottom(unknown).

:- pred create_argument(yn_t, yn_t, yn_t, yn_t, yn_t, state).
create_argument(yes,   _,   _,   _,   _, ground) :- !. % Ground argument.
create_argument( no, yes,  no, yes,   _, uninit) :- !. % Non-duplicated uninit.
create_argument( no, yes,  no,   _,  no, uninit) :- !. % First occurrence.
create_argument( no, yes,   _,  no, yes, any) :- !.    % Already initialized.
create_argument( no, yes, yes,   _,   _, any) :- !.    % Duplicated argument.
create_argument( no,  no,   _,   _,   _, any) :- !.    % Non-variable argument.

:- pred lub_call(functor_t, functor_t, functor_t).
lub_call(Call1, Call2, Lub) :-
	functor(Call1, Na, Ar),
	functor(Call2, Na, Ar),
	functor(Lub, Na, Ar),
	lub_call(1, Ar, Call1, Call2, Lub).

:- pred lub_call(integer, integer, functor_t, functor_t, functor_t).
lub_call(I, Ar, _, _, _) :- I>Ar, !.
lub_call(I, Ar, Call1, Call2, Lub) :- I=<Ar, !,
	arg(I, Call1, X1),
	arg(I, Call2, X2),
	arg(I, Lub, X),
	lub(X1, X2, X),
	I1 is I+1,
	lub_call(I1, Ar, Call1, Call2, Lub).

:- pred bottom_call(gamma_t, functor_t).
bottom_call(Na/Ar, Bottom) :-
	functor(Bottom, Na, Ar),
	bottom_call(1, Ar, Bottom).

:- pred bottom_call(integer, integer, functor_t).
bottom_call(I, Ar, _Bottom) :- I>Ar, !.
bottom_call(I, Ar, Bottom) :- I=<Ar, !,
	bottom(B),
	arg(I, Bottom, B),
	I1 is I+1,
	bottom_call(I1, Ar, Bottom).

:- pred lattice_modes_call(gamma_t, node(integer,beta_t,node,node), clause).
lattice_modes_call(Na/Ar, Table, (Head:-Formula)) :-
	functor(Head, Na, Ar),
	get(Table, Na/Ar, Value),
	lattice_modes_call(1, Ar, Value, Head, Formula, true).

:- pred lattice_modes_call(integer, integer, functor_t, functor_t, epsilon_t, epsilon_t).
lattice_modes_call(I, Ar, _, _, Link, Link) :- I>Ar, !.
lattice_modes_call(I, Ar, Value, Head, Formula, Link) :- I=<Ar, !,
	arg(I, Value, T),
	arg(I, Head, X),
	lattice_modes_arg(T, X, Formula, Mid),
	I1 is I+1,
	lattice_modes_call(I1, Ar, Value, Head, Mid, Link).

:- pred lattice_modes_arg(state, any, epsilon_t, epsilon_t).
lattice_modes_arg(uninit, X, (uninit(X),Link), Link) :- !.
lattice_modes_arg(ground, X, (ground(X),Link), Link) :- !.
lattice_modes_arg(_Other, _, Link, Link).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Table utilities:

% This code implements a mutable array, represented as a binary tree.

% Access a value in logarithmic time and constant space:
% This predicate can be used to create the array incrementally.
:- pred get(node(integer,beta_t,node,node), gamma_t, functor_t).
get(node(N,to_beta_t(W),to_node(L),to_node(R)), to_gamma_t(I), V) :- get(N, W, L, R, to_gamma_t(I), V).

:- pred get(integer, functor_t, node(integer,beta_t,node,node), node(integer,beta_t,node,node), gamma_t, functor_t).
get(N, V, _, _, to_gamma_t(I), V) :- I=N, !.
get(N, _, L, R, to_gamma_t(I), V) :-
	compare(Order, I, N),
	get(Order, to_gamma_t(I), V, L, R).

:- pred get(cmp, gamma_t, functor_t, node(integer,beta_t,node,node), node(integer,beta_t,node,node)).
get(<, to_gamma_t(I), V, L, _) :- get(L, to_gamma_t(I), V).
get(>, to_gamma_t(I), V, _, R) :- get(R, to_gamma_t(I), V).

:- pred set(node(integer,beta_t,node,node), integer, functor_t, node(integer,beta_t,node,node)).
set(leaf,          I, to_functor_t(V), node(I,V,leaf,leaf)).
set(node(N,W,to_node(L),to_node(R)), I, V, node(N,NW,to_node(NL),to_node(NR))) :-
	compare(Order, I, N),
	set_2(Order, I, V, to_functor_t(W), L, R, to_functor_t(NW), NL, NR).

:- pred set_2(cmp, integer, functor_t, functor_t, node(integer,beta_t,node,node), node(integer,beta_t,node,node), functor_t, node(integer,beta_t,node,node), node(integer,beta_t,node,node)).
set_2(<, I, V, W, L, R, W, NL, R) :- set(L, I, V, NL).
set_2(=, _, V, _, L, R, V, L, R).
set_2(>, I, V, W, L, R, W, L, NR) :- set(R, I, V, NR).

% Prevent any further insertions in the array:
:- pred seal(node(integer,beta_t,node,node)).
seal(leaf).
seal(node(_,_,to_node(L),to_node(R))) :- seal(L), seal(R).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% General utilities:

:- pred membership_flag(functor_t, list(delta_t), yn_t).
membership_flag(X, Set, yes) :- inv(to_delta_t(X), Set), !.
membership_flag(_, _et,  no).

:- pred ground_flag(functor_t, list(delta_t), yn_t).
ground_flag(X, Ground, yes) :- varset(X, Set), subsetv(Set, Ground), !.
ground_flag(_, _round,  no).

:- pred get_entry_modes(state, functor_t, zeta_t, list(delta_t)).
get_entry_modes(Type, Head, Value, TypeSet) :-
	functor(Head, _, Ar),
	get_entry_modes(Type, 1, Ar, Head, Value, Bag),
	sort(Bag, TypeSet)::sort(list(any),list(delta_t)).

:- pred get_entry_modes(state, integer, integer, functor_t, zeta_t, list(any)).
get_entry_modes(_, I, Ar, _, _, []) :- I>Ar, !.
get_entry_modes(T, I, Ar, Head, Value, [X|Bag]) :- I=<Ar, arg(I, Value, T), !,
	arg(I, Head, X),
	I1 is I+1,
	get_entry_modes(T, I1, Ar, Head, Value, Bag).
get_entry_modes(T, I, Ar, Head, Value, Bag) :- I=<Ar, !,
	I1 is I+1,
	get_entry_modes(T, I1, Ar, Head, Value, Bag).

:- pred var_args(functor_t, list(delta_t)).
var_args(Goal, Set) :-
	functor(Goal, _, Ar),
	filter_vars(Ar, Goal, Bag),
	sort(Bag, Set)::sort(list(any),list(delta_t)).

:- pred filter_vars(integer, functor_t, list(any)).
filter_vars(Ar, Goal, Vs) :- filter_vars(Ar, Goal, Vs, []).

%:- pred filter_vars(integer, any).
:- pred filter_vars(integer, functor_t, list(any), list(any)).
filter_vars(N, _oal) --> {N=<0}, !.
filter_vars(N, Goal) --> {N>0}, !,
	{arg(N, Goal, V)},
	filter_vars_arg(N, Goal, V).

%:- pred filter_vars_arg(integer, any, any).
filter_vars_arg(N, Goal, V) --> {var(V)}, !, [V],
	{N1 is N-1},
	filter_vars(N1, Goal).
filter_vars_arg(N, Goal, V) --> {nonvar(V)}, !,
	{N1 is N-1},
	filter_vars(N1, Goal).

:- pred goal_dupset(functor_t, list(delta_t)).
goal_dupset(Goal, DupSet) :-
	goal_dupset_varbag(Goal, DupSet, _).

:- pred goal_dupset_varset(functor_t, list(delta_t), list(delta_t)).
goal_dupset_varset(Goal, DupSet, VarSet) :-
	goal_dupset_varbag(Goal, DupSet, VarBag),
	sort(VarBag, VarSet)::sort(list(delta_t),list(delta_t)).

:- pred goal_dupset_varbag(functor_t, list(delta_t), list(delta_t)).
goal_dupset_varbag(Goal, DupSet, VarBag) :-
	varbag(Goal, VarBag),
	make_key(VarBag, KeyBag),
	keysort(KeyBag, KeySet),
	filter_dups(KeySet, DupSet).

:- pred make_key(list(delta_t), list(any)).
make_key([], []).
make_key([V|Bag], [V-dummy|KeyBag]) :- make_key(Bag, KeyBag).

:- pred filter_dups(list(any), list(delta_t)).
filter_dups(KeySet, Set) :- filter_dups(KeySet, Set, []).

%:- pred filter_dups(any).
:- pred filter_dups(list(any), list(delta_t), list(any)).
filter_dups([]) --> !.
filter_dups([V1-_,V2-_,V3-_|KeySet]) --> {V1==V2,V2==V3}, !,
	filter_dups([V2-_,V3-_|KeySet]).
filter_dups([V1-_,V2-_|KeySet]) --> {V1==V2}, !,
	[V1], filter_dups(KeySet).
filter_dups([_-_|KeySet]) --> !,
	filter_dups(KeySet).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Low-level utilities:

:- pred set_command(eta_t, list(delta_t), list(delta_t)).
set_command(sub(X), In, Out) :- diffv(In, [X], Out).
set_command(add(X), In, Out) :- includev(X, In, Out).
set_command(sub_set(X), In, Out) :- diffv(In, X, Out).
set_command(add_set(X), In, Out) :- unionv(X, In, Out).

:- pred table_command(accessor, node(integer,beta_t,node,node), node(integer,beta_t,node,node)).
table_command(get(I,Val), In,  In) :- get(In, I, Val).
table_command(set(I,Val), In, Out) :- set(In, I, Val, Out).

% Set utilities inspired by R. O'Keefe in Practical Prolog:
:- pred inv(delta_t, list(delta_t)).
inv(A, [B|S]) :-
	compare(Order, A, B),
	inv_2(Order, A, S).

:- pred inv_2(cmp, delta_t, list(delta_t)).
inv_2(=, _, _).
inv_2(>, A, S) :- inv(A, S).

:- pred intersectv(list(delta_t), list(delta_t), list(delta_t)).
intersectv([], _, []).
intersectv([A|S1], S2, S) :- intersectv_2(S2, A, S1, S).

:- pred intersectv_2(list(delta_t), delta_t, list(delta_t), list(delta_t)).
intersectv_2([], _, _, []).
intersectv_2([B|S2], A, S1, S) :-
	compare(Order, A, B),
	intersectv_3(Order, A, S1, B, S2, S).

:- pred intersectv_3(cmp, delta_t, list(delta_t), delta_t, list(delta_t), list(delta_t)).
intersectv_3(<, _, S1, B, S2,     S) :- intersectv_2(S1, B, S2, S).
intersectv_3(=, A, S1, _, S2, [A|S]) :- intersectv(S1, S2, S).
intersectv_3(>, A, S1, _, S2,     S) :- intersectv_2(S2, A, S1, S).

:- pred diffv(list(delta_t), list(delta_t), list(delta_t)).
diffv([], _, []).
diffv([A|S1], S2, S) :- diffv_2(S2, A, S1, S).

:- pred diffv_2(list(delta_t), delta_t, list(delta_t), list(delta_t)).
diffv_2([], A, _, [A]).
diffv_2([B|S2], A, S1, S) :-
	compare(Order, A, B),
	diffv_3(Order, A, S1, B, S2, S).

:- pred diffv_3(cmp, delta_t, list(delta_t), delta_t, list(delta_t), list(delta_t)).
diffv_3(<, A, S1, B, S2, [A|S]) :- diffv(S1, [B|S2], S).
diffv_3(=, _, S1, _, S2,     S) :- diffv(S1, S2, S).
diffv_3(>, A, S1, _, S2,     S) :- diffv_2(S2, A, S1, S).

:- pred unionv(list(delta_t), list(delta_t), list(delta_t)).
unionv([], S2, S2).
unionv([A|S1], S2, S) :- unionv_2(S2, A, S1, S).

:- pred unionv_2(list(delta_t), delta_t, list(delta_t), list(delta_t)).
unionv_2([], A, S1, [A|S1]).
unionv_2([B|S2], A, S1, S) :-
	compare(Order, A, B),
	unionv_3(Order, A, S1, B, S2, S).

:- pred unionv_3(cmp, delta_t, list(delta_t), delta_t, list(delta_t), list(delta_t)).
unionv_3(<, A, S1, B, S2, [A|S]) :- unionv_2(S1, B, S2, S).
unionv_3(=, A, S1, _, S2, [A|S]) :- unionv(S1, S2, S).
unionv_3(>, A, S1, B, S2, [B|S]) :- unionv_2(S2, A, S1, S).

:- pred includev(delta_t, list(delta_t), list(delta_t)).
includev(A, S1, S) :- includev_2(S1, A, S).

:- pred includev_2(list(delta_t), delta_t, list(delta_t)).
includev_2([], A, [A]).
includev_2([B|S1], A, S) :-
	compare(Order, A, B),
	includev_3(Order, A, B, S1, S).

:- pred includev_3(cmp, delta_t, delta_t, list(delta_t), list(delta_t)).
includev_3(<, A, B, S1, [A,B|S1]).
includev_3(=, _, B, S1, [B|S1]).
includev_3(>, A, B, S1, [B|S]) :- includev_2(S1, A, S).

:- pred subsetv(list(delta_t), list(delta_t)).
subsetv([], _).
subsetv([A|S1], [B|S2]) :-
	compare(Order, A, B),
	subsetv_2(Order, A, S1, S2).

:- pred subsetv_2(cmp, delta_t, list(delta_t), list(delta_t)).
subsetv_2(=, _, S1, S2) :- subsetv(S1, S2).
subsetv_2(>, A, S1, S2) :- subsetv([A|S1], S2).

:- pred varset(functor_t, list(delta_t)).
varset(Term, VarSet) :- varbag(Term, VB), sort(VB, VarSet)::sort(list(delta_t),list(delta_t)).
:- pred varbag(functor_t, list(delta_t)).
varbag(Term, VarBag) :- varbag(Term, VarBag, []).

%:- pred varbag(any).
:- pred varbag(functor_t, list(delta_t), list(any)).
varbag(Var) --> {var(Var)}, !, [Var].
varbag(Str) --> {nonvar(Str), !, functor(Str,_,Arity)}, varbag(Str, 1, Arity).

%:- pred varbag(functor_t, integer, integer).
varbag(_Str, N, Arity) --> {N>Arity}, !.
varbag(Str, N, Arity) --> {N=<Arity}, !,
        {arg(N, Str, Arg)}, varbag(Arg),
        {N1 is N+1},
        varbag(Str, N1, Arity).

:- pred unify_p(functor_t).
unify_p(_=_).

:- pred call_p(functor_t).
call_p(G) :- \+unify_p(G).

:- pred split_unify(functor_t, functor_t, functor_t).
split_unify(X=Y, X, Y).
split_unify(Y=X, X, Y).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

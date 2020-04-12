% preprocessing phase to eliminate disjunctions from the code

% takes a list of clauses of the form source(Name,Clause)
% returns these clauses with disjunctions replaced by dummy calls
% and a list of NewClauses corresponding to those dummy calls
% Link is the uninstantiated last cdr of this list

:- style_check(-singleton).
:- use_module(type_check).
:- style_check(+singleton).

:- ensure_loaded(harness).

:- type empty.
:- type clause_head ---> a(any, any, any).
:- type clause_body --->  to_clause_body((b(any) ; c(any))) ; (clause_body, clause_body) ; \+(any) ; \=(any,any) ; not(any) ; to_clause_body(any) ;
	to_clause_body((clause_body ; clause_body)) ; to_clause_body((any -> any ; any)) ;
	to_clause_body(((any=any,!,fail);true)) ; to_clause_body(((any,!,fail);true)) ; to_clause_body(((any,!,any);true)).
:- type clause ---> (clause_head :- clause_body).
:- type list(integer) ---> "A".
:- type variable ---> '_dummy_' ; to_variable(term_t0).
:- type term_t0 ---> [term_t0 | term_t0] ; to_term_t0(any) ; to_term_t0((clause_body;clause_body)).
:- type term_t1 ---> (list(clause), list(clause)) ; to_term_t1(clause).
:- type symbol_table ---> [] ; [p(variable, any) | symbol_table] ; [p(term_t1, term_t1) | symbol_table].
:- type disj_t ---> disj(clause_body, integer, clause_body, term_t0) ; disj(clause_body, integer, clause_head, clause).

:- trust_pred atomic(term_t0).
:- trust_pred name(variable, list(integer)).
:- trust_pred append(list(integer), list(integer), list(integer)).
:- trust_pred sort(list(variable), list(variable)).

:- pred data(empty).
data(_).

:- pred benchmark(empty, empty).
benchmark(_Data, _Out) :-
	eliminate_disjunctions([(a(A,_,C):-to_clause_body((b(A);c(C))))],X,Y,[]),
    inst_vars((X,Y)).

:- pred eliminate_disjunctions(list(clause), list(clause), list(clause), list(clause)).
eliminate_disjunctions(OneProc,NewProc,NewClauses,Link) :-
	gather_disj(OneProc,NewProc,Disj,[]),
    treat_disj(Disj,NewClauses,Link).

:- pred gather_disj(list(clause), list(clause), list(disj_t), list(disj_t)).
gather_disj([],[],Link,Link).
gather_disj([C|Cs],NewProc,Disj,Link) :-
	extract_disj(C, NewC, Disj, Rest),
	NewProc = [NewC|NewCs],
	gather_disj(Cs,NewCs,Rest,Link).

% given a clause, find in Disj the list of disj((A;B),N,X,C)
% where N is a unique ID, X is a var that takes the place of
% (A;B) in the code, NewC is the clause modified in such a way that
% the disjunctions are replaced by the corresponding vars
% Link is the last (uninstantiated) cdr of the list Disj.
% do the work of pretrans for nots, -> etc...
% put all those guys inside disjunctions
:- pred extract_disj(clause, clause, list(disj_t), list(disj_t)).
extract_disj(C, (Head:-NewBody), Disj, Link) :-
	C = (Head:-Body), !,
	CtrIn = 0,
	extract_disj(Body, NewBody, Disj, Link, C, CtrIn, _CtrOut).
extract_disj(Head, Head, Link, Link).

:- pred extract_disj(clause_body, clause_body, list(disj_t), list(disj_t), clause, integer, integer).
extract_disj((C1,C2), (NewC1,NewC2), Disj, Link, C, CtrIn, CtrOut) :-
	extract_disj(C1, NewC1, Disj, Link1, C, CtrIn, Ctr),
	extract_disj(C2, NewC2, Link1, Link, C, Ctr, CtrOut).

extract_disj(Goal, X, Disj, Link, C, CtrIn, CtrOut) :-
	is_disj(Goal,NewGoal), !,
	Disj = [disj(NewGoal,CtrIn,X,C)|Link],
	CtrOut is CtrIn + 1.
extract_disj(Goal, Goal, Link, Link, _, CtrIn, CtrIn).

:- pred is_disj(clause_body, clause_body).
is_disj(to_clause_body(((C1 -> C2); C3)),to_clause_body(((C1, !, C2); C3))) :- !.
is_disj(to_clause_body((C1;C2)),to_clause_body((C1;C2))).
is_disj(not(C),to_clause_body(((C,!,fail);true))).
is_disj(\+(C),to_clause_body(((C,!,fail);true))).
is_disj('\\='(C1,C2),to_clause_body(((C1 = C2,!,fail);true))).

% given a list of disj((A;B),N,X,C), for each, do the following:
% 1) find vars in (A;B)
% 2) find the vars in C
% 3) intersect the two sets of vars into one list
% 4) make a predicate name using N as a part of it ('dummy_disjN')
% 5) put a structure with that name and those vars as args
% 6) binds X to this call
% 7) add new clauses [(dummy:-A)),(dummy:-B))]
:- pred treat_disj(list(disj_t), list(clause), list(clause)).
treat_disj([], Link, Link).
treat_disj([disj(to_clause_body((A;B)),N,X,C)|Disjs], DummyClauses, Link) :-
	find_vars(to_term_t0((A;B)),Vars),
	find_vars(C,CVars),
	intersect_vars(Vars,CVars,Args),
	make_dummy_name(N,Name),
	(X =.. [Name|Args])::(clause_head =.. list(variable)),
	make_dummy_clauses(to_clause_body((A;B)),X,DummyClauses,Rest),
	treat_disj(Disjs, Rest, Link).

:- pred make_dummy_clauses(clause_body, clause_head, list(clause), list(clause)).
make_dummy_clauses(to_clause_body((A;B)),X,[NewC|Cs],Link) :- 
	!,
	copy(to_term_t1((X:-A)), to_term_t1(NewC)),
	make_dummy_clauses(B,X,Cs,Link).
make_dummy_clauses(A,X,[NewC|Link],Link) :- copy(to_term_t1((X:-A)),to_term_t1(NewC)).

:- pred find_vars(term_t0, list(variable)).
find_vars(X,Y) :- find_vars(X,Y,Link), Link = [].

:-pred find_vars(term_t0, list(variable), list(variable)). % consider revising typeof(Link)!
find_vars(Var,[to_variable(Var)|Link],Link) :- var(Var), !.
find_vars(Cst,Link,Link) :- atomic(Cst), !.
find_vars([T|Ts],Vars,NewLink) :- !,
	find_vars(T,Vars,Link),
	find_vars(Ts,Link,NewLink).
find_vars(Term,Vars,Link) :-
	(Term =.. [_|Args])::(term_t0 =.. any),
	find_vars(Args,Vars,Link).

:- pred intersect_vars(list(variable), list(variable), list(variable)).
intersect_vars(V1,V2,Out) :-
	sort_vars(V1,Sorted1),
	sort_vars(V2,Sorted2),
	intersect_sorted_vars(Sorted1,Sorted2,Out).

:- pred make_dummy_name(variable, variable).
make_dummy_name(N,Name) :-
	name('_dummy_',L1),
	name(N,L2),
	append(L1,L2,L),
	name(Name,L).

% copy_term using a symbol table.
:- pred copy(term_t1, term_t1).
copy(Term1, Term2) :-
        varset(Term1, Set), make_sym(Set, Sym),
        copy2(Term1, Term2, Sym), !.

:- pred copy2(term_t1, term_t1, symbol_table).
copy2(V1, V2, Sym) :- var(V1), !, retrieve_sym(V1, Sym, V2).
copy2(X1, X2, Sym) :- nonvar(X1), !,
        functor(X1,Name,Arity),
        functor(X2,Name,Arity),
        copy2(X1, X2, Sym, 1, Arity).

:- pred copy2(term_t1, term_t1, symbol_table, integer, integer).
copy2(_X1,_X2,_Sym, N, Arity) :- N>Arity, !.
copy2(X1, X2, Sym, N, Arity) :- N=<Arity, !,
        arg(N, X1, Arg1),
        arg(N, X2, Arg2),
        copy2(Arg1, Arg2, Sym),
        N1 is N+1,
        copy2(X1, X2, Sym, N1, Arity).

:- pred retrieve_sym(term_t1, symbol_table, term_t1).
retrieve_sym(V, [p(W,X)|_Sym], X) :- V==W, !.
retrieve_sym(V, [_|Sym], X) :- retrieve_sym(V, Sym, X).

:- pred make_sym(list(variable), symbol_table).
make_sym([], []).
make_sym([V|L], [p(V,_)|S]) :- make_sym(L, S).

% *** Gather all variables used in a term: (in a set or a bag)
:- pred varset(term_t1, list(variable)).
varset(Term, VarSet) :- varbag(Term, VB), 
    sort(VB, VarSet).

:- pred varbag(term_t1, list(variable)).
varbag(Term, VarBag) :- varbag(Term, VarBag, []).

:- pred varbag(term_t1, list(variable), list(any)).
varbag(Var) --> {var(Var)}, !, [Var].
varbag(Str) --> {nonvar(Str), !, functor(Str,_,Arity)}, varbag(Str, 1, Arity).

:- pred varbag(term_t1, integer, integer, list(variable), list(any)). % unchecked!
varbag(_Str, N, Arity) --> {N>Arity}, !.
varbag(Str, N, Arity) --> {N=<Arity}, !,
        {arg(N, Str, Arg)}, varbag(Arg),
        {N1 is N+1},
        varbag(Str, N1, Arity).

:- pred inst_vars(term_t1).
inst_vars(Term) :-
	varset(Term, Vars),
        [A]="A",
	inst_vars_list(Vars, A).

:- pred inst_vars_list(list(variable), integer).
inst_vars_list([], _).
inst_vars_list([T|L], N) :-
	name(T, [N]),
	N1 is N+1,
	inst_vars_list(L, N1).

:- pred sort_vars(list(variable), list(variable)).
sort_vars(V,Out) :- sort_vars(V,Out,[]).

:- pred sort_vars(list(variable), list(variable), list(variable)).
sort_vars([],Link,Link).
sort_vars([V|Vs],Result,Link) :-
	split_vars(Vs,V,Smaller,Bigger),
	sort_vars(Smaller,Result,[V|SLink]),
	sort_vars(Bigger,SLink,Link).

:- pred intersect_sorted_vars(list(variable), list(variable), list(variable)).
intersect_sorted_vars([],_,[]) :- !.
intersect_sorted_vars(_,[],[]).
intersect_sorted_vars([X|Xs],[Y|Ys],[X|Rs]) :-
	X == Y, !,
	intersect_sorted_vars(Xs,Ys,Rs).
intersect_sorted_vars([X|Xs],[Y|Ys],Rs) :-
	X @< Y, !,
	intersect_sorted_vars(Xs,[Y|Ys],Rs).
intersect_sorted_vars([X|Xs],[Y|Ys],Rs) :-
	X @> Y, !,
	intersect_sorted_vars([X|Xs],Ys,Rs).

:- pred split_vars(list(variable), variable, list(variable), list(variable)).
split_vars([],_,[],[]).
split_vars([V|Vs],A,[V|Ss],Bs) :-
	V @< A, !,
	split_vars(Vs,A,Ss,Bs).
split_vars([V|Vs],A,Ss,Bs) :-
	V == A, !,
	split_vars(Vs,A,Ss,Bs).
split_vars([V|Vs],A,Ss,[V|Bs]) :-
	V @> A, !,
	split_vars(Vs,A,Ss,Bs).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% A Graph Reducer for T-Combinators:
% Reduces a T-combinator expression to a final answer.  Recognizes
% the combinators I,K,S,B,C,S',B',C', cond, apply, arithmetic, tests,
% basic list operations, and function definitions in the data base stored
% as facts of the form t_def(_func, _args, _expr).
% Written by Peter Van Roy

:- ensure_loaded(harness).

:- style_check(-singleton).

:- type unk0_t ---> i ; i(integer) ; 'true' ; 'false' ; [any | '.'] ; [any, any | '.'] ; cond(any, integer, any) ; cond(any, list(any), any) ; append(any, any) ; to_unk0_t(any) ; to_unk0_t(unk3_t).
:- type operator ---> '+' ; '-' ; '*' ; '//' ; 'mod' ; '-' ; '<' ; '>' ; '=<' ; '>=' ; '=\\=' ; '=:='.
:- type unk1_t ---> 'fac' ; 'quick' ; 'quick2' ; 'split' ; 'inserthead' ; 'inserttail' ; 'append' ; to_unk1_t(unk3_t).
:- type unk2_t ---> 'sp' ; 'bp' ; 'cp' ; 's' ; 'b' ; 'c' ; 'k' ; 'i' ; 'cond' ; 'apply' ; 'hd' ; 'tl'.
:- type unk3_t ---> [] ; [any | unk2_t] ; [any, any | unk2_t] ; [any, any, any | unk2_t] ; to_unk3_t(unk0_t) ; to_unk3_t(operator).
%:- type beta_t ---> any ; [].
:- type alpha_t ---> any ; [[]] ; [any]. %to_unk4_t(any) ; [] ; [any].

:- trust_pred atomic(unk0_t).
:- trust_pred atom(unk3_t).
:- trust_pred member(operator, list(operator)).
:- trust_pred number(unk0_t).

:- pred benchmark(unk0_t, unk0_t).
benchmark([X1,X2], [Y1,Y2]) :-
	try(X1, Y1),
	try(X2, Y2).

:- pred data(list(any)).
data([fac(3), quick([3,1,2])]).

:- pred try(unk0_t, unk0_t).
try(_inpexpr, _anslist) :-
	listify(_inpexpr, _list),
	curry(_list, _curry),
	t_reduce(_curry, _ans), 
	make_list(_ans, _anslist).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Examples of applicative functions which can be compiled & executed.
% This test version compiles them just before each execution.

:- pred t_def(unk1_t, any, unk0_t).

% Factorial function:
t_def(fac, [N], cond(N=0, 1, N*fac(N-1))).

% Quicksort:
t_def(quick, [_l], cond(_l=[], [],
		 cond(tl(_l)=[], _l,
		 quick2(split(hd(_l),tl(_l)))))).
t_def(quick2, [_l], append(quick(hd(_l)), quick(tl(_l)))).

t_def(split, [_e,_l], cond(_l=[], [[_e]|[]],
		    cond(hd(_l)=<_e, inserthead(hd(_l),split(_e,tl(_l))),
		    inserttail(hd(_l),split(_e,tl(_l)))))).
t_def(inserthead, [_e,_l], [[_e|hd(_l)]|tl(_l)]).
t_def(inserttail, [_e,_l], [hd(_l)|[_e|tl(_l)]]).

t_def(append, [_a,_b], cond(_a=[], _b, [hd(_a)|append(tl(_a),_b)])).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Full reduction:
% A dot '.' is printed for each reduction step.

:- pred t_reduce(unk0_t, unk0_t).
t_reduce(_expr, _ans) :-
	atomic(_expr), !,
	 _ans=_expr.
% The reduction of '.' must be here to avoid an infinite loop
t_reduce([_y,_x|'.'], [_yr,_xr|'.']) :-
	t_reduce(_x, _xr),
	!,
	t_reduce(_y, _yr),
	!.
t_reduce(_expr, _ans) :-
	t_append(_next, _red, _form, _expr),
%	write('.'),
	t_redex(to_unk3_t(_form), _red),
	!,
	t_reduce(_next, _ans), 
	!.

:- pred t_append(unk0_t, unk0_t, unk0_t, unk0_t).
t_append(_link, _link, _l, _l).
t_append([_a|_l1], _link, _l2, [_a|_l3]) :- t_append(_l1, _link, _l2, _l3).

% One step of the reduction:

:- pred t_redex(unk3_t, unk0_t).

% Combinators:
t_redex([_x,_g,_f,_k|sp], [[_xr|_g],[_xr|_f]|_k]) :- t_reduce(_x, _xr).
t_redex([_x,_g,_f,_k|bp], [[_x|_g],_f|_k]).
t_redex([_x,_g,_f,_k|cp], [_g,[_x|_f]|_k]).
t_redex([_x,_g,_f|s], [[_xr|_g]|[_xr|_f]]) :- t_reduce(_x, _xr).
t_redex([_x,_g,_f|b], [[_x|_g]|_f]).
t_redex([_x,_g,_f|c], [_g,_x|_f]).
t_redex([_y,_x|k], _x).
t_redex([_x|i], _x).

% Conditional:
t_redex([_elsepart,_ifpart,_cond|cond], _ifpart) :-
	t_reduce(_cond, _bool), _bool=true, !.
	% Does NOT work if _bool is substituted in the call!
t_redex([_elsepart,_ifpart,_cond|cond], _elsepart).

% Apply:
t_redex([_f|apply], _fr) :- 
	t_reduce(_f, _fr).

% List operations:
t_redex([_arg|hd], _x) :- 
	t_reduce(_arg, [_y,_x|'.']).
t_redex([_arg|tl], _y) :- 
	t_reduce(_arg, [_y,_x|'.']).

% Arithmetic:
t_redex([_y,_x|_op], _res) :-
	atom(to_unk3_t(_op)),
	member(_op, ['+', '-', '*', '//', 'mod']),
	t_reduce(_x, _xres),
	t_reduce(_y, _yres),
	number(_xres), number(_yres),
	eval(_op, _res, _xres, _yres).

% Tests:
t_redex([_y,_x|_test], _res) :-
	atom(to_unk3_t(_test)),
	member(_test, ['<', '>', '=<', '>=', '=\\=', '=:=']),
	t_reduce(_x, _xres),
	t_reduce(_y, _yres),
	number(_xres), number(_yres),
	(relop(_test, _xres, _yres)
	-> _res=true
	;  _res=false
	), !.

% Equality:
t_redex([_y,_x|=], _res) :-
	t_reduce(_x, _xres),
	t_reduce(_y, _yres),
	(_xres=_yres -> _res=true; _res=false), !.

% Arithmetic functions:
t_redex([_x|_op], _res) :-
	atom(to_unk3_t(_op)),
	member(_op, ['-']),
	t_reduce(_x, _xres),
	number(_xres),
	eval1(_op, _t, _xres).

% Definitions:
% Assumes a fact t_def(_func,_def) in the database for every
% defined function.
t_redex(_in, _out) :-
	concatenate(_par,_func,_in),
	atom(_func),
	t_def(to_unk1_t(_func), _args, _expr),
	t(_args, _expr, _def),
	concatenate(_par,_def,to_unk3_t(_out)).

:- pred concatenate(unk3_t, unk3_t, unk3_t).
concatenate([], L, L).
concatenate([X|L1], L2, [X|L3]) :- concatenate(L1, L2, L3).

% Basic arithmetic and relational operators:

:- pred eval(operator, unk0_t, unk0_t, unk0_t).
eval(  '+', i(C), i(A), i(B)) :- C is A + B.
eval(  '-', i(C), i(A), i(B)) :- C is A - B.
eval(  '*', i(C), i(A), i(B)) :- C is A * B.
eval( '//', i(C), i(A), i(B)) :- C is A // B.
eval('mod', i(C), i(A), i(B)) :- C is A mod B.

:- pred eval1(operator, unk0_t, unk0_t).
eval1('-', i(C), i(A)) :- C is -A.

:- pred relop(operator, unk0_t, unk0_t).
relop(  '<', i(A), i(B)) :- A<B.
relop(  '>', i(A), i(B)) :- A>B.
relop( '=<', i(A), i(B)) :- A=<B.
relop( '>=', i(A), i(B)) :- A>=B.
relop('=\\=', i(A), i(B)) :- A=\=B.
relop('=:=', i(A), i(B)) :- A=:=B.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Scheme T:
% A Translation Scheme for T-Combinators

% Translate an expression to combinator form
% by abstracting out all variables in _argvars:
:- pred t(any, unk0_t, unk3_t).
t(_argvars, _expr, _trans) :-
	listify(_expr, _list),
	curry(_list, _curry),
	t_argvars(_argvars, _curry, to_unk0_t(_trans)), !.

:- pred t_argvars(any, unk0_t, unk0_t).
t_argvars([], _trans, _trans).
t_argvars([_x|_argvars], _in, _trans) :-
	t_argvars(_argvars, _in, _mid),
	t_vars(_mid, _vars), % calculate variables in each subexpression
	t_trans(_x, _mid, _vars, _trans). % main translation routine

% Curry the original expression:
% This converts an applicative expression of any number
% of arguments and any depth of nesting into an expression
% where all functions are curried, i.e. all function
% applications are to one argument and have the form
% [_arg|_func] where _func & _arg are also of that form.
% Input is a nested function application in list form.
% Currying makes t_trans faster.
:- pred curry(unk0_t, unk0_t).
curry(_a, _a) :- (var(_a); atomic(_a)), !.
curry([_func|_args], _cargs) :-
	currylist(_args, _cargs, _func).

% Transform [_a1, ..., _aN] to [_cN, ..., _c1|_link]-_link
:- pred currylist(any, unk0_t, unk0_t).
currylist([], _link, _link) :- !.
currylist([_a|_args], _cargs, _link) :-
	curry(_a, _c),
	currylist(_args, _cargs, [_c|_link]).

% Calculate variables in each subexpression:
% To any expression a list of the form
% [_vexpr, _astr, _fstr] is matched.
% If the expression is a variable or an atom
% then this list only has the first element.
% _vexpr = List of all variables in the expression.
% _astr, _fstr = Similar structures for argument & function.
:- pred t_vars(unk0_t, alpha_t).
t_vars(_v, [[_v]]) :- var(_v), !.
t_vars(_a, [[]]) :- atomic(_a), !.
t_vars([_func], [[]]) :- atomic(_func), !.
t_vars([_arg|_func], [_g,[_g1|_af1],[_g2|_af2]]) :-
	t_vars(_arg, [_g1|_af1]),
	t_vars(_func, [_g2|_af2]),
	unionv(_g1, _g2, _g).

% The main translation routine:
% trans(_var, _curriedexpr, _varexpr, _result)
% The translation scheme T in the article is followed literally.
% A good example of Prolog as a specification language.
:- pred t_trans(unk0_t, unk0_t, alpha_t, unk0_t).
t_trans(_x, _a, _, [_a|k]) :- (atomic(_a); var(_a), _a\==_x), !.
t_trans(_x, _y, _, i) :- _x==_y, !.
t_trans(_x, _e, [_ve|_], [_e|k]) :- notinv(_x, _ve).
t_trans(_x, [_f|_e], [_vef,_sf,_se], _res) :-
	_sf=[_vf|_],
	_se=[_ve|_other],
	(atom(_e); _other=[_,[_ve1|_]], _ve1\==[]),
	t_rule1(_x, _e, _ve, _se, _f, _vf, _sf, _res).
t_trans(_x, [_g|[_f|_e]], [_vefg,_sg,_sef], _res) :-
	_sg=[_vg|_],
	_sef=[_vef,_sf,_se],
	_se=[_ve|_],
	_sf=[_vf|_],
	t_rule2(_x, _e, _f, _vf, _sf, _g, _vg, _sg, _res).

% First complex rule of translation scheme T:
:- pred t_rule1(unk0_t, unk3_t, any, alpha_t, unk0_t, any, alpha_t, unk0_t).
t_rule1(_x, _e, _ve, _se, _f, _vf, _sf, to_unk0_t(_e)) :-
	notinv(_x, _ve), _x==_f, !.
t_rule1(_x, _e, _ve, _se, _f, _vf, _sf, [_resf,_e|b]) :-
	notinv(_x, _ve), inv(_x, _vf), _x\==_f, !,
	t_trans(_x, _f, _sf, _resf).
t_rule1(_x, _e, _ve, _se, _f, _vf, _sf, [_f,_rese|c]) :-
	/* inv(_x, _ve), */ 
	notinv(_x, _vf), !,
	t_trans(_x, to_unk0_t(_e), _se, _rese).
t_rule1(_x, _e, _ve, _se, _f, _vf, _sf, [_resf,_rese|s]) :-
	/* inv(_x, _ve), inv(_x, _vf), */
	t_trans(_x, to_unk0_t(_e), _se, _rese),
	t_trans(_x, _f, _sf, _resf).

% Second complex rule of translation scheme T:
:- pred t_rule2(unk0_t, any, unk0_t, any, alpha_t, unk0_t, any, alpha_t, unk0_t).
t_rule2(_x, _e, _f, _vf, _sf, _g, _vg, _sg, [_g,_e|c]) :-
	_x==_f, notinv(_x, _vg), !.
t_rule2(_x, _e, _f, _vf, _sf, _g, _vg, _sg, [_resg,_e|s]) :-
	_x==_f, /* inv(_x, _vg), */ !,
	t_trans(_x, _g, _sg, _resg).
t_rule2(_x, _e, _f, _vf, _sf, _g, _vg, _sg, [_g,_resf,_e|cp]) :-
	/* _x\==_f, */ inv(_x, _vf), notinv(_x, _vg), !,
	t_trans(_x, _f, _sf, _resf).
t_rule2(_x, _e, _f, _vf, _sf, _g, _vg, _sg, [_resg,_resf,_e|sp]) :-
	/* _x\==_f, */ inv(_x, _vf), /* inv(_x, _vg), */ !,
	t_trans(_x, _f, _sf, _resf),
	t_trans(_x, _g, _sg, _resg).
t_rule2(_x, _e, _f, _vf, _sf, _g, _vg, _sg, [_f|_e]) :-
	/* notinv(_x, _vf), */ _x==_g, !.
t_rule2(_x, _e, _f, _vf, _sf, _g, _vg, _sg, [_resg,_f,_e|bp]) :-
	/* notinv(_x, _vf), inv(_x, _vg), _x\==_g, */
	t_trans(_x, _g, _sg, _resg).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% List utilities:

% Convert curried list into a regular list:
:- pred make_list(unk0_t, unk0_t).
make_list(_a, _a) :- atomic(_a).
make_list([_b,_a|'.'], [_a|_rb]) :- make_list(_b, _rb).

:- pred listify(unk0_t, unk0_t).
listify(_X, _X) :- 
	(var(_X); atomic(_X)), !.
listify(_Expr, [_Op|_LArgs]) :-
	functor(_Expr, _Op, N),
	listify_list(1, N, _Expr, _LArgs).

:- pred listify_list(integer, integer, unk0_t, any).
listify_list(I, N, _, []) :- I>N, !.
listify_list(I, N, _Expr, [_LA|_LArgs]) :- I=<N, !,
	arg(I, _Expr, _A),
	listify(_A, _LA),
	I1 is I+1,
	listify_list(I1, N, _Expr, _LArgs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Set utilities:
% Implementation inspired by R. O'Keefe, Practical Prolog.
% Sets are represented as sorted lists without duplicates.
% Predicates with 'v' suffix work with sets containing uninstantiated vars.

% *** Intersection
:- pred intersectv(any, any, any).
intersectv([], _, []).
intersectv([A|S1], S2, S) :- intersectv_2(S2, A, S1, S).

:- pred intersectv_2(any, any, any, any).
intersectv_2([], _, _, []).
intersectv_2([B|S2], A, S1, S) :-
        compare(Order, A, B),
        intersectv_3(Order, A, S1, B, S2, S).

:- pred intersectv_3(cmp, any, any, any, any, any).
intersectv_3(<, _, S1, B, S2,     S) :- intersectv_2(S1, B, S2, S).
intersectv_3(=, A, S1, _, S2, [A|S]) :- intersectv(S1, S2, S).
intersectv_3(>, A, S1, _, S2,     S) :- intersectv_2(S2, A, S1, S).

:- pred intersectv_list(any, any).
intersectv_list([], []).
intersectv_list([InS|Sets], OutS) :- intersectv_list(Sets, InS, OutS).

:- pred intersectv_list(any, any, any).
intersectv_list([]) --> [].
intersectv_list([S|Sets]) --> intersectv(S), intersectv_list(Sets).

% *** Difference
:- pred diffv(any, any, any).
diffv([], _, []).
diffv([A|S1], S2, S) :- diffv_2(S2, A, S1, S).

:- pred diffv_2(any, any, any, any).
diffv_2([], A, S1, [A|S1]).
diffv_2([B|S2], A, S1, S) :-
        compare(Order, A, B),
        diffv_3(Order, A, S1, B, S2, S).
 
:- pred diffv_3(cmp, any, any, any, any, any).
diffv_3(<, A, S1, B, S2, [A|S]) :- diffv(S1, [B|S2], S).
diffv_3(=, _A, S1, _, S2,     S) :- diffv(S1, S2, S).
diffv_3(>, A, S1, _, S2,     S) :- diffv_2(S2, A, S1, S).
 
% *** Union
:- pred unionv(any, any, any).
unionv([], S2, S2).
unionv([A|S1], S2, S) :- unionv_2(S2, A, S1, S).

:- pred unionv_2(any, any, any, any).
unionv_2([], A, S1, [A|S1]).
unionv_2([B|S2], A, S1, S) :-
        compare(Order, A, B),
        unionv_3(Order, A, S1, B, S2, S).

:- pred unionv_3(cmp, any, any, any, any, any).
unionv_3(<, A, S1, B, S2, [A|S]) :- unionv_2(S1, B, S2, S).
unionv_3(=, A, S1, _, S2, [A|S]) :- unionv(S1, S2, S).
unionv_3(>, A, S1, B, S2, [B|S]) :- unionv_2(S2, A, S1, S).
 
% *** Subset
:- pred subsetv(any, any).
subsetv([], _).
subsetv([A|S1], [B|S2]) :-
        compare(Order, A, B),
        subsetv_2(Order, A, S1, S2).

:- pred subsetv_2(cmp, any, any, any).
subsetv_2(=, _, S1, S2) :- subsetv(S1, S2).
subsetv_2(>, A, S1, S2) :- subsetv([A|S1], S2).
 
% For unordered lists S1:
:- pred small_subsetv(any, any).
small_subsetv([], _).
small_subsetv([A|S1], S2) :- inv(A, S2), small_subsetv(S1, S2).
 
% *** Membership
:- pred inv(unk0_t, any).
inv(A, [B|S]) :-
        compare(Order, A, B),
        inv_2(Order, A, S).

:- pred inv_2(cmp, unk0_t, any).
inv_2(=, _, _).
inv_2(>, A, S) :- inv(A, S).

% *** Non-membership
:- pred notinv(unk0_t, any).
notinv(A, S) :- notinv_2(S, A).

:- pred notinv_2(any, unk0_t).
notinv_2([], _).
notinv_2([B|S], A) :-
        compare(Order, A, B),
        notinv_3(Order, A, S).

:- pred notinv_3(cmp, unk0_t, any).
notinv_3(<, _, _).
notinv_3(>, A, S) :- notinv_2(S, A).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

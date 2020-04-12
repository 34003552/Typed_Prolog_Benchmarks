% generated: 20 November 1989
% option(s): 
%
%   boyer
%
%   Evan Tick (from Lisp version by R. P. Gabriel)
%
%   November 1985
%
%   prove arithmetic theorem

:- style_check(-singleton).
:- use_module(type_check).
:- style_check(+singleton).

:- ensure_loaded(harness).

:- type w_bool ---> 't' ; 'f' ;
	and(w_bool) ; and(w_bool, w_bool) ; assignedp(any, any) ; 
	divides(w_int, w_int) ;
	eqp(w_int, w_int) ; equal(w_int, w_int) ; equal(w_bool, w_bool) ;
	f(any) ;
	if(w_bool, w_bool, w_bool) ; if(w_bool, w_bool, w_bool, w_bool) ; iff(w_bool, w_bool) ; implies(w_bool, w_bool) ;
	lessp(w_bool, w_bool) ; lessp(w_int, w_int) ; listp(any) ; 
	boyer_member(w_int, w_int) ;
	nlistp(any) ; not(w_bool) ; numberp(w_int) ; 
	or(w_bool, w_bool) ; 
	prime(w_int) ; prime1(w_int, w_int) ; prime_list(any) ;
	zerop(w_int).
 :- type w_int ---> w_bool ; zero ; [] ; i(integer) ;
	add1(w_int) ; append(w_int, w_int) ; 
	cons(any, any) ; count_list(any, any) ;
	decr(w_int) ; delete(w_int, w_int) ; boyer_difference(w_int, w_int) ;
	exp(any, w_int) ;
	fix(w_int) ; flatten(any) ;
	gcd(w_int, w_int) ; greatest_factor(w_int, w_int) ; 
	if(w_bool, w_int, w_int) ; intersect(w_int, w_int) ;
	length(w_int) ; lessp(w_bool, w_bool) ; 
	meaning(w_int, w_int) ;
	nth(w_int, w_int) ;
	plus(w_int, w_int) ; plus_fringe(w_int) ; plus_tree(w_int) ; power_eval(any, any) ;
	quotient(w_int, w_int) ;
	sort2(w_int) ;
	times(w_int, w_int) ; times_list(w_int) ;
	remainder(w_int, w_int) ; reverse(w_int) ; reverse_loop(any, list(any)).

:- type equal_t0 ---> to_equal_t0(w_bool) ;
	and(w_bool, w_bool) ; append(w_int, w_int) ; assignment(any, any) ; assume_false(any, any) ; assume_true(any, any) ;
	boolean(w_int) ;
	car(any) ; compile(any) ; count_list(any, any) ; countps_(any, any) ;
	boyer_difference(w_int, w_int) ; divides(w_int, w_int) ; dsort(any) ;
	eqp(w_int, w_int) ; equal(w_int, w_int) ; even1(w_int) ; exec(any, any, any) ; exp(any, w_int) ; 
	fact_(any) ; falsify(any) ; fix(any) ; flatten(any) ;
	gcd(w_int, w_int) ; get(w_int, any) ; greatereqp(any, any) ; greatereqpr(any, any) ; greaterp(any, any) ;
	if(w_bool, any, any) ; iff(w_bool, w_bool) ; implies(w_bool, w_bool) ;
	last(any) ; length(w_int) ; lesseqp(any, any) ; lessp(w_int, w_int) ; listp(any) ;
	mc_flatten(any, w_int) ; meaning(w_int, w_int) ; boyer_member(w_int, w_int) ;
	not(w_bool) ; nth(w_int, w_int) ; numberp(w_int) ;
	or(w_bool, w_bool) ;
	plus(w_int, w_int) ; power_eval(any, any) ; prime(w_int) ; prime_list(any) ;
	quotient(w_int, w_int) ;
	remainder(w_int, w_int) ; reverse_(any) ; reverse(w_int) ; reverse_loop(any, list(any)) ;
	samefringe(any, any) ; sigma(w_int, w_int) ; sort2(w_int) ;
	tautology_checker(any) ; times(w_int, w_int) ; times_list(any) ;
	value(any, any) ;
	zerop(w_int).

:- type equal_t1 ---> to_equal_t1(w_int) ; to_equal_t1(w_bool) ;
	append(w_int, w_int) ; assignedp(any, any) ; and(w_bool, w_bool) ;
	cons(any, any) ; count_list(any, any) ; countps_loop(any, any, any) ;
	delete(w_int, w_int) ; 
	equal(w_int, w_int) ; exec(any, any, any) ;
	fact_loop(any, integer) ; falsify1(any, any) ;
	if(w_bool, w_int, w_int) ; if(w_bool, w_bool, w_bool) ; if(w_bool, w_bool, w_bool, w_bool) ; if(w_bool, w_bool, w_int, w_int) ; intersect(w_int, w_int) ;
	listp(any) ; lessp(w_bool, w_bool) ; lessp(w_int, w_int) ;
	not(any) ;
	or(w_bool, w_bool) ;
	plus(w_int, w_int) ;
	quotient(w_int, w_int) ;
	reverse(any) ; reverse_loop(any, list(any)) ;
	sort2(any) ;
	tautologyp(any, any) ; times(w_int, w_int) ;
	value(any, any) ;
	zerop(w_int).

:- trust_pred atomic(w_bool).

:- pred benchmark(w_bool, w_bool).
benchmark(Wff, NewWff) :-
	rewrite(Wff,NewWff),
	tautology(NewWff,[],[]).

:- pred data(w_bool).
data(implies(and(implies(X,Y),
                and(implies(Y,Z),
                    and(implies(Z,U),
                        implies(U,W)))),
            implies(X,W))) :-
        X = f(plus(plus(a,b),plus(c,zero))),
        Y = f(times(times(a,b),plus(c,d))),
        Z = f(reverse(append(append(a,b),[]))),
        U = equal(plus(a,b),boyer_difference(x,y)),
        W = lessp(remainder(a,b),boyer_member(a,length(b))).

:- pred tautology(w_bool).
tautology(Wff) :-
%        write('rewriting...'),nl,
        rewrite(Wff,NewWff),
%        write('proving...'),nl,
        tautology(NewWff,[],[]).

:- pred tautology(w_bool, list(w_bool), list(w_bool)).
tautology(Wff,Tlist,Flist) :-
        (truep(Wff,Tlist) -> true
        ;falsep(Wff,Flist) -> fail
        ;Wff = if(If,Then,Else) ->
		(truep(If,Tlist) -> tautology(Then,Tlist,Flist)
		;falsep(If,Flist) -> tautology(Else,Tlist,Flist)
		;tautology(Then,[If|Tlist],Flist),	% both must hold
		 tautology(Else,Tlist,[If|Flist])
                )
        ),!.

:- pred rewrite(w_bool, w_bool).
rewrite(Atom,Atom) :-
        atomic(Atom),!.
rewrite(Old,New) :-
        functor(Old,F,N),
        functor(Mid,F,N),
        rewrite_args(N,Old,Mid),
        ( equal(to_equal_t0(Mid),to_equal_t1(Next)),        % should be ->, but is compiler smart
          rewrite(Next,New)       % enough to generate cut for -> ?
        ; New=Mid
        ),!.

:- pred rewrite_args(integer, w_bool, w_bool).
rewrite_args(0,_,_) :- !.
rewrite_args(N,Old,Mid) :- 
        arg(N,Old,OldArg),
        arg(N,Mid,MidArg),
        rewrite(OldArg,MidArg),
        N1 is N-1,
        rewrite_args(N1,Old,Mid).

:- pred truep(w_bool, list(w_bool)).
truep(t,_) :- !.
truep(Wff,Tlist) :- boyer_member(Wff,Tlist).

:- pred falsep(w_bool, list(w_bool)).
falsep(f,_) :- !.
falsep(Wff,Flist) :- boyer_member(Wff,Flist).

:- pred boyer_member(w_bool, list(w_bool)).
boyer_member(X,[X|_]) :- !.
boyer_member(X,[_|T]) :- boyer_member(X,T).

:- pred equal(equal_t0, equal_t1).
equal(  and(P,Q),
        if(P,if(Q,t,f),f)
        ).
equal(  append(append(X,Y),Z),
        append(X,append(Y,Z))
        ).
equal(  assignment(X,append(A,B)),
        if(assignedp(X,A),
           assignment(X,A),
           assignment(X,B))
        ).
equal(  assume_false(Var,Alist),
        cons(cons(Var,f),Alist)
        ).
equal(  assume_true(Var,Alist),
        cons(cons(Var,t),Alist)
        ).
equal(  boolean(X),
        or(equal(X,t),equal(X,f))
        ).
equal(  car(gopher(X)),
        if(listp(X),
        car(flatten(X)),
        zero)
        ).
equal(  compile(Form),
        reverse(codegen(optimize(Form),[]))
        ).
equal(  count_list(Z,sort_lp(X,Y)),
        plus(count_list(Z,X),
             count_list(Z,Y))
        ).
equal(  countps_(L,Pred),
        countps_loop(L,Pred,zero)
        ).
equal(  boyer_difference(A,B),
        to_equal_t1(C)
        ) :- boyer_difference(A,B,C).
equal(  divides(X,Y),
        zerop(remainder(Y,X))
        ).
equal(  dsort(X),
        sort2(X)
        ).
equal(  eqp(X,Y),
        equal(fix(X),fix(Y))
        ).
equal(  equal(A,B),
        to_equal_t1(C)
        ) :- eq(A,B,C).
equal(  even1(X),
        if(zerop(X),t,odd(decr(X)))
        ).
equal(  exec(append(X,Y),Pds,Envrn),
        exec(Y,exec(X,Pds,Envrn),Envrn)
        ).
equal(  exp(A,B),
        to_equal_t1(C)
        ) :- exp(A,B,C).
equal(  fact_(I),
        fact_loop(I,1)
        ).
equal(  falsify(X),
        falsify1(normalize(X),[])
        ).
equal(  fix(X),
        if(numberp(X),X,zero)
        ).
equal(  flatten(cdr(gopher(X))),
        if(listp(X),
           cdr(flatten(X)),
           cons(zero,[]))
        ).
equal(  gcd(A,B),
        to_equal_t1(C)
        ) :- gcd(A,B,C).
equal(  get(J,set(I,Val,Mem)),
        if(eqp(J,I),Val,get(J,Mem))
        ).
equal(  greatereqp(X,Y),
        not(lessp(X,Y))
        ).
equal(  greatereqpr(X,Y),
        not(lessp(X,Y))
        ).
equal(  greaterp(X,Y),
        lessp(Y,X)
        ).
equal(  if(if(A,B,C),D,E),
        if(A,if(B,D,E),if(C,D,E))
        ).
equal(  iff(X,Y),
        and(implies(X,Y),implies(Y,X))
        ).
equal(  implies(P,Q),
        if(P,if(Q,t,f),t)
        ).
equal(  last(append(A,B)),
        if(listp(B),
           last(B),
           if(listp(A),
              cons(car(last(A))),
              B))
        ).
equal(  length(A),
        to_equal_t1(B)
        ) :- mylength(A,B).
equal(        lesseqp(X,Y),
        not(lessp(Y,X))
        ).
equal(  lessp(A,B),
        to_equal_t1(C)
        ) :- lessp(A,B,C).
equal(  listp(gopher(X)),
        listp(X)
        ).
equal(  mc_flatten(X,Y),
        append(flatten(X),Y)
        ).
equal(  meaning(A,B),
        to_equal_t1(C)
        ) :- meaning(A,B,C).
equal(  boyer_member(A,B),
        to_equal_t1(C)
        ) :- myboyer_member(A,B,C).
equal(  not(P),
        if(P,f,t)
        ).
equal(  nth(A,B),
        to_equal_t1(C)
        ) :- nth(A,B,C).
equal(  numberp(greatest_factor(X,Y)),
        not(and(or(zerop(Y),equal(Y,i(1))),
                not(numberp(X))))            
        ).
equal(  or(P,Q),
        if(P,t,if(Q,t,f),f)
        ).
equal(  plus(A,B),
        to_equal_t1(C)
        ) :- plus(A,B,C).
equal(  power_eval(A,B),
        to_equal_t1(C)
        ) :- power_eval(A,B,C).
equal(  prime(X),
        and(not(zerop(X)),
            and(not(equal(X,add1(zero))),
                prime1(X,decr(X))))
        ).
equal(  prime_list(append(X,Y)),
        and(prime_list(X),prime_list(Y))
        ).
equal(  quotient(A,B),
        to_equal_t1(C)
        ) :- quotient(A,B,C).
equal(  remainder(A,B),
        to_equal_t1(C)
        ) :- remainder(A,B,C).
equal(  reverse_(X),
        reverse_loop(X,[])
        ).
equal(  reverse(append(A,B)),
        append(reverse(B),reverse(A))
        ).
equal(  reverse_loop(A,B),
        to_equal_t1(C)
        ) :- reverse_loop(A,B,C).
equal(  samefringe(X,Y),
        equal(flatten(X),flatten(Y))
        ).
equal(  sigma(zero,I),
        quotient(times(I,add1(I)),i(2))
        ).
equal(  sort2(delete(X,L)),
        delete(X,sort2(L))
        ).
equal(  tautology_checker(X),
        tautologyp(normalize(X),[])
        ).
equal(  times(A,B),
        to_equal_t1(C)
        ) :- times(A,B,C).
equal(  times_list(append(X,Y)),
        times(times_list(X),times_list(Y))
        ).
equal(  value(normalize(X),A),
        value(X,A)
        ).
equal(  zerop(X),
        or(equal(X,zero),not(numberp(X)))
        ).

:- pred boyer_difference(w_int, w_int, w_int).
boyer_difference(X, X, zero) :- !.
boyer_difference(plus(X,Y), X, fix(Y)) :- !.
boyer_difference(plus(Y,X), X, fix(Y)) :- !.
boyer_difference(plus(X,Y), plus(X,Z), boyer_difference(Y,Z)) :- !.
boyer_difference(plus(B,plus(A,C)), A, plus(B,C)) :- !.
boyer_difference(add1(plus(Y,Z)), Z, add1(Y)) :- !.
boyer_difference(add1(add1(X)), i(2), fix(X)).

:- pred eq(w_int, w_int, w_bool).
eq(plus(A,B), zero, and(zerop(A),zerop(B))) :- !.
eq(plus(A,B), plus(A,C), equal(fix(B),fix(C))) :- !.
eq(zero, boyer_difference(X,Y),not(lessp(Y,X))) :- !.
eq(X, boyer_difference(X,Y),and(numberp(X),
                          and(or(equal(X,zero),
                                 zerop(Y))))) :- !.
eq(times(X,Y), zero, or(zerop(X),zerop(Y))) :- !.
eq(append(A,B), append(A,C), equal(B,C)) :- !.
eq(flatten(X), cons(Y,[]), and(nlistp(X),equal(X,Y))) :- !.
eq(greatest_factor(X,Y),zero, and(or(zerop(Y),equal(Y,i(1))),
                                     equal(X,zero))) :- !.
eq(greatest_factor(X,_),i(1), equal(X,i(1))) :- !.
eq(Z, times(W,Z), and(numberp(Z),
                      or(equal(Z,zero),
                         equal(W,i(1))))) :- !.
eq(X, times(X,Y), or(equal(X,zero),
                     and(numberp(X),equal(Y,i(1))))) :- !.
eq(times(A,B), i(1), and(not(equal(A,zero)),
                      and(not(equal(B,zero)),
                          and(numberp(A),
                              and(numberp(B),
                                  and(equal(decr(A),zero),
                                      equal(decr(B),zero))))))) :- !.
eq(boyer_difference(X,Y), boyer_difference(Z,Y),if(lessp(X,Y),
                                       not(lessp(Y,Z)),
                                       if(lessp(Z,Y),
                                          not(lessp(Y,X)),
                                          equal(fix(X),fix(Z))))) :- !.
eq(lessp(X,Y), Z, if(lessp(X,Y),
                     equal(t,Z),
                     equal(f,Z))).

:- pred exp(any, w_int, w_int).
exp(I, plus(J,K), times(exp(I,J),exp(I,K))) :- !.
exp(I, times(J,K), exp(exp(I,J),K)).

:- pred gcd(w_int, w_int, w_int).
gcd(X, Y, gcd(Y,X)) :- !.
gcd(times(X,Z), times(Y,Z), times(Z,gcd(X,Y))).

:- pred mylength(w_int, w_int).
mylength(reverse(X),length(X)).
mylength(cons(_,cons(_,cons(_,cons(_,cons(_,cons(_,X7)))))),
         plus(i(6),length(X7))).

:- pred lessp(w_int, w_int, w_bool).
lessp(remainder(_,Y), Y, not(zerop(Y))) :- !.
lessp(quotient(I,J), I, and(not(zerop(I)),
                            or(zerop(J),
                               not(equal(J,i(1)))))) :- !.
lessp(remainder(X,Y), X, and(not(zerop(Y)),
                             and(not(zerop(X)),
                                 not(lessp(X,Y))))) :- !.
lessp(plus(X,Y), plus(X,Z), lessp(Y,Z)) :- !.
lessp(times(X,Z), times(Y,Z), and(not(zerop(Z)),
                                  lessp(X,Y))) :- !.
lessp(Y, plus(X,Y), not(zerop(X))) :- !.
lessp(length(delete(X,L)), length(L), boyer_member(X,L)).

:- pred meaning(w_int, w_int, w_int).
meaning(plus_tree(append(X,Y)),A,
        plus(meaning(plus_tree(X),A),
             meaning(plus_tree(Y),A))) :- !.
meaning(plus_tree(plus_fringe(X)),A,
        fix(meaning(X,A))) :- !.
meaning(plus_tree(delete(X,Y)),A,
        if(boyer_member(X,Y),
           boyer_difference(meaning(plus_tree(Y),A),
                      meaning(X,A)),
           meaning(plus_tree(Y),A))).

:- pred myboyer_member(w_int, w_int, w_bool).
myboyer_member(X,append(A,B),or(boyer_member(X,A),boyer_member(X,B))) :- !.
myboyer_member(X,reverse(Y),boyer_member(X,Y)) :- !.
myboyer_member(A,intersect(B,C),and(boyer_member(A,B),boyer_member(A,C))).

:- pred nth(w_int, w_int, w_int).
nth(zero,_,zero).
nth([],I,if(zerop(I),[],zero)).
nth(append(A,B),I,append(nth(A,I),nth(B,boyer_difference(I,length(A))))).

:- pred plus(w_int, w_int, w_int).
plus(plus(X,Y),Z,
     plus(X,plus(Y,Z))) :- !.
plus(remainder(X,Y),
     times(Y,quotient(X,Y)),
     fix(X)) :- !.
plus(X,add1(Y),
     if(numberp(Y),
        add1(plus(X,Y)),
        add1(X))).

:- pred power_eval(any, any, w_int).
power_eval(big_plus1(L,I,Base),Base,
           plus(power_eval(L,Base),I)) :- !.
power_eval(power_rep(I,Base),Base,
           fix(I)) :- !.
power_eval(big_plus(X,Y,I,Base),Base,
           plus(I,plus(power_eval(X,Base),
                       power_eval(Y,Base)))) :- !.
power_eval(big_plus(power_rep(I,Base),
                    power_rep(J,Base),
                    zero,
                    Base),
           Base,
           plus(I,J)).

:- pred quotient(w_int, w_int, w_int).
quotient(plus(X,plus(X,Y)),i(2),plus(X,quotient(Y,i(2)))).
quotient(times(Y,X),Y,if(zerop(Y),zero,fix(X))).

:- pred remainder(w_int, w_int, w_int).
remainder(_,         i(1),zero) :- !.
remainder(X,         X,zero) :- !.
remainder(times(_,Z),Z,zero) :- !.
remainder(times(Y,_),Y,zero).

:- pred reverse_loop(any, list(any), any).
reverse_loop(X,Y,  append(reverse(X),Y)) :- !.
reverse_loop(X,[], reverse(X)          ).

:- pred times(w_int, w_int, w_int).
times(X,         plus(Y,Z),      plus(times(X,Y),times(X,Z))      ) :- !.
times(times(X,Y),Z,              times(X,times(Y,Z))              ) :- !.
times(X,         boyer_difference(C,W),boyer_difference(times(C,X),times(W,X))) :- !.
times(X,         add1(Y),        if(numberp(Y),
                                    plus(X,times(X,Y)),
                                    fix(X))                       ).
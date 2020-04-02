%   poly_10
%
%   Ralph Haygood (based on Prolog version by Rick McGeer
%                  based on Lisp version by R. P. Gabriel)
%
%   raise a polynomial (1+x+y+z) to the 10th power (symbolically)

:- style_check(-singleton).
:- use_module(type_check).
:- style_check(+singleton).

:- ensure_loaded(harness).

:- type poly_var ---> x ; y ; z.
:- type term ---> term(integer, poly).
:- type poly ---> poly(poly_var, list(term)) ; p(integer).

:- pred data(poly).
data(Data) :-
	test_poly(Data).

:- pred benchmark(poly, poly).
benchmark(Data, Out) :-
	poly_exp(10, Data, Out).

:- pred test_poly1(poly).
test_poly1(P) :-
	P = poly(x, [term(0,p(1)), term(1,p(1))]).

:- pred test_poly2(poly).
test_poly2(P) :-
	P = poly(y, [term(1,p(1))]).

:- pred test_poly3(poly).
test_poly3(P) :-
	P = poly(z, [term(1,p(1))]).

:- pred test_poly(poly).
test_poly(P) :-
	poly_add(poly(x, [term(0,p(1)), term(1,p(1))]), poly(y, [term(1, p(1))]), Q),
	poly_add(poly(z, [term(1,p(1))]), Q, P).

:- pred poly_add(poly, poly, poly).
poly_add(poly(Var,Terms1), poly(Var,Terms2), poly(Var,Terms)) :- !,
	term_add(Terms1, Terms2, Terms).
poly_add(poly(Var1,Terms1), poly(Var2,Terms2), poly(Var1,Terms)) :-
	Var1 @< Var2, !,
	add_to_order_zero_term(Terms1, poly(Var2,Terms2), Terms).
poly_add(Poly, poly(Var,Terms2), poly(Var,Terms)) :- !,
	add_to_order_zero_term(Terms2, Poly, Terms).
poly_add(poly(Var,Terms1), C, poly(Var,Terms)) :- !,
	add_to_order_zero_term(Terms1, C, Terms).
poly_add(p(C1), p(C2), p(C)) :-
	C is C1 + C2.

:- pred term_add(list(term), list(term), list(term)).
term_add([], X, X) :- !.
term_add(X, [], X) :- !.
term_add([term(E,C1)|Terms1], [term(E,C2)|Terms2], [term(E,C)|Terms]) :- !,
	poly_add(C1, C2, C),
	term_add(Terms1, Terms2, Terms).
term_add([term(E1,C1)|Terms1], [term(E2,C2)|Terms2], [term(E1,C1)|Terms]) :-
	E1 < E2, !,
	term_add(Terms1, [term(E2,C2)|Terms2], Terms).
term_add(Terms1, [term(E2,C2)|Terms2], [term(E2,C2)|Terms]) :-
	term_add(Terms1, Terms2, Terms).

:- pred add_to_order_zero_term(list(term), poly, list(term)).
add_to_order_zero_term([term(0,C1)|Terms], C2, [term(0,C)|Terms]) :- !,
	poly_add(C1, C2, C).
add_to_order_zero_term(Terms, C, [term(0,C)|Terms]).

:- pred poly_exp(integer, poly, poly).
poly_exp(0, _, p(1)) :- !.
poly_exp(N, Poly, Result) :-
	N/\1 =:= 0, !,
	M is N>>1,
	poly_exp(M, Poly, Part),
	poly_mul(Part, Part, Result).
poly_exp(N, Poly, Result) :-
	M is N - 1,
	poly_exp(M, Poly, Part),
	poly_mul(Poly, Part, Result).

:- pred poly_mul(poly, poly, poly).
poly_mul(poly(Var,Terms1), poly(Var,Terms2), poly(Var,Terms)) :- !,
	term_mul(Terms1, Terms2, Terms).
poly_mul(poly(Var1,Terms1), poly(Var2,Terms2), poly(Var1,Terms)) :-
	Var1 @< Var2, !,
	mul_through(Terms1, poly(Var2,Terms2), Terms).
poly_mul(P, poly(Var,Terms2), poly(Var,Terms)) :- !,
	mul_through(Terms2, P, Terms).
poly_mul(poly(Var,Terms1), C, poly(Var,Terms)) :- !,
	mul_through(Terms1, C, Terms).
poly_mul(p(C1), p(C2), p(C)) :-
	C is C1 * C2.

:- pred term_mul(list(term), list(term), list(term)).
term_mul([], _, []) :- !.
term_mul(_, [], []) :- !.
term_mul([Term|Terms1], Terms2, Terms) :-
	single_term_mul(Terms2, Term, PartA),
	term_mul(Terms1, Terms2, PartB),
	term_add(PartA, PartB, Terms).

:- pred single_term_mul(list(term), term, list(term)).
single_term_mul([], _, []).
single_term_mul([term(E1,C1)|Terms1], term(E2,C2),
		[term(E,C)|Terms]) :-
	E is E1 + E2,
	poly_mul(C1, C2, C),
	single_term_mul(Terms1, term(E2,C2), Terms).

:- pred mul_through(list(term), poly, list(term)).
mul_through([], _, []).
mul_through([term(E,Term)|Terms], Poly, [term(E,NewTerm)|NewTerms]) :-
	poly_mul(Term, Poly, NewTerm),
	mul_through(Terms, Poly, NewTerms).

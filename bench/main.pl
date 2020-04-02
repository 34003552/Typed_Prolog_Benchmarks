(:-set_prolog_flag(redefine_warnings,off)).

test(X,Y) :- write('Loading benchmark: '), write(X), nl, [X], bench(Y), unload_file(X).

(:-test('bad/boyer.pl', 100)).
(:-test('bad/browse.pl', 100)).
(:-test('bad/chat_parser.pl',100)).
(:-test('good/crypt.pl',10000)).
(:-test('good/deriv.pl',100000)).
(:-test('good/dynamic_unit_clause.pl',10000)).
(:-test('good/fast_mu.pl',10000)).
(:-test('bad/flatten.pl',100000)).
(:-test('good/itak.pl',100)).
(:-test('good/meta_qsort.pl',5000)).
(:-test('good/mu.pl',10000)).
(:-test('good/nreverse.pl',100000)).
(:-test('good/nreverse_builtin.pl',100000)).
(:-test('good/poly.pl',1000)).
(:-test('good/primes.pl',10000)).
(:-test('good/prover.pl',10000)).
(:-test('good/qsort.pl',100000)).
(:-test('good/queens.pl',10000)).
(:-test('good/query.pl',100000)).
(:-test('bad/reducer.pl',500)).
(:-test('good/sendmore.pl',500)).
(:-test('bad/simple_analyzer.pl',500)).
(:-test('good/tak.pl',1000)).
(:-test('bad/unify.pl',20000)).
(:-test('good/zebra.pl',500)).
(:-halt).
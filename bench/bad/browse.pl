% generated: 19 June 1990
% option(s): 
%
%   browse
%
%   Tep Dobry (from Lisp version by R. P. Gabriel)
%
%   (modified January 1987 by Herve' Touati)

:- style_check(-singleton).
:- use_module(type_check).
%:- style_check(+singleton).

:- ensure_loaded(harness).

:- type empty.
:- type ab ---> a ; b.
:- type nested_list(T) ---> [] ; [T | list(T)] ; [T | list(list(T))] ; [list(T) | list(list(T))].
:- type pattern ---> dummy(any) ; pattern(nested_list(ab)).
:- type unk ---> any ; star(unk2) ; list(any).
:- type unk2 ---> [] ; [unk2 | unk2] ; [unk | list(unk2)] ; [unk2 | list(unk2)] ; to_unk2(list(unk)) ; to_unk2(list(any)).

:- trust_pred length(list(_), integer).
:- trust_pred append(unk2,unk2,unk2). %:- trust_pred append(list(T),list(T),list(T)).
:- trust_pred atom(_).

:- pred data(empty).
data(_).

:- pred benchmark(empty, empty).
benchmark(_Data, _Out) :-
    init(100,10,4,
         [[a,a,a,b,b,b,b,a,a,a,a,a,b,b,a,a,a],
          [a,a,b,b,b,b,a,a,[a,a],[b,b]],
          [a,a,a,b,[b,a],b,a,b,a]
         ],
         Symbols),
    randomize(Symbols,RSymbols,21),!,
    investigate(RSymbols,
                [[star(SA),B,star(SB),B,a,star(SA),a,star(SB),star(SA)],
                 [star(SA),star(SB),star(SB),star(SA),[star(SA)],[star(SB)]],
                 [_,_,star(_),[b,a],star(_),_,_]
                ]).

:- pred init(integer, integer, integer, nested_list(ab), list(list(pattern))).
init(N,M,Npats,Ipats,Result) :- init(N,M,M,Npats,Ipats,Result).

:- pred init(integer, integer, integer, integer, nested_list(ab), list(list(pattern))).
init(0,_,_,_,_,_) :- !.
init(N,I,M,Npats,Ipats,[Symb|Rest]) :- 
    fill(I,[],L),
    get_pats(Npats,Ipats,Ppats),
    J is M - I,
    fill(J,[pattern(Ppats)|L],Symb),
    N1 is N - 1,
    (I =:= 0 -> I1 is M; I1 is I - 1),
    init(N1,I1,M,Npats,Ipats,Rest).

:- pred fill(integer, list(pattern), list(pattern)).
fill(0,L,L) :- !.
fill(N,L,[dummy([])|Rest]) :- 
    N1 is N - 1,
    fill(N1,L,Rest).

:- pred randomize(list(list(pattern)), list(list(pattern)), integer).
randomize([],[],_) :- !.
randomize(In,[X|Out],Rand) :-
    length(In,Lin),
    Rand1 is (Rand * 17) mod 251,
    N is Rand1 mod Lin,
    split(N,In,X,In1),
    randomize(In1,Out,Rand1).

:- pred split(integer, list(list(pattern)), list(pattern), list(list(pattern))).
split(0,[X|Xs],X,Xs) :- !.
split(N,[X|Xs],RemovedElt,[X|Ys]) :-
    N1 is N - 1,
    split(N1,Xs,RemovedElt,Ys).

:- pred investigate(list(list(pattern)), list(list(any))).
investigate([],_) :- !.
investigate([U|Units],Patterns) :-
    property(U,pattern,Data),
    p_investigate(Data,Patterns),
    investigate(Units,Patterns).

:- pred get_pats(integer, nested_list(ab), nested_list(ab)).
get_pats(Npats,Ipats,Result) :- get_pats(Npats,Ipats,Result,Ipats).

:- pred get_pats(integer, nested_list(ab), nested_list(ab), nested_list(ab)).
get_pats(0,_,[],_) :- !.
get_pats(N,[X|Xs],[X|Ys],Ipats) :-
    N1 is N - 1,
    get_pats(N1,Xs,Ys,Ipats).
get_pats(N,[],Ys,Ipats) :-
    get_pats(N,Ipats,Ys,Ipats).

:- pred property(list(pattern), any, list(list(unk))).
property([],_,_) :- fail.	/* don't really need this */
property([Prop|_],P,Val) :-
    functor(Prop,P,_),!,
    arg(1,Prop,Val).
property([_|RProps],P,Val) :-
    property(RProps,P,Val).

:- pred p_investigate(list(list(unk)), list(list(any))).
p_investigate([],_).
p_investigate([D|Data],Patterns) :-
    p_match(Patterns,D),
    p_investigate(Data,Patterns).

:- pred p_match(list(list(any)), list(unk)).
p_match([],_).
p_match([P|Patterns],D) :-
    (match(to_unk2(D),to_unk2(P)),fail; true),
    p_match(Patterns,D).

:- pred match(unk2, unk2).
match([],[]) :- !.
match([X|PRest],[Y|SRest]) :-
    var(Y),!,X = Y,
    match(PRest,SRest).
match(List,[Y|Rest]) :- 
    nonvar(Y), Y = star(X),!,
    append(X,SRest,List),
    match(SRest,Rest).
match([X|PRest],[Y|SRest]) :-
    (atom(X) -> X = Y; match(X,Y)),
    match(PRest,SRest).


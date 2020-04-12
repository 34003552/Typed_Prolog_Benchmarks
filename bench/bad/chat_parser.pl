% generated: 19 November 1989
% option(s): 
%
%  chat_parser
%
%  Fernando C. N. Pereira and David H. D. Warren

:- style_check(-singleton).
:- use_module(type_check).
%:- style_check(+singleton).

:- ensure_loaded(harness).

:- set_prolog_flag(single_var_warnings, off).

:- type empty.
:- type word_t --->
	a ; afghanistan ; african ; all ; am ; american ; an ; and ; 'any' ; anybody ; anyone ; anything ; are ; area ; areas ; as ; at ; asian ; australasia ; average ; averages ;
	baltic ; be ; being ; been ; big ; bigger ; biggest ; black_sea ; border ; bordered ; bordering ; borders ; by ;
	capital ; capitals ; china ; cities ; city ; contain ; contained ; containing ; contains ; continent ; continents ; countries ; country ;
	danube ; degree ; degrees ; did ; do ; does ; doing ; done ; drain ; drained ; draining ; drains ; 
	each ; east ; eight ; equator ; european ; every ; everybody ; everything ; everyone ; exceed ; exceeded ; exceeding ; exceeds ;
	five ; flow ; flowed ; flows ; flowing ; four ; from ;
	great ; greater ; 
	had ; has ; have ; having ; he ; her ; him ; his ; how ;
	i ; in ; into ; is ; it ; its ;
	ksqmile ; ksqmiles ;
	large ; larger ; largest ; latitude ; latitudes ; least ; less ; london ; longitude ; longitudes ;
	many ; maximum ; me ; million ; minimum ; more ; most ; my ;
	nb(integer) ; new ; newer ; newest ; nine ; no ; nobody ; north ; not ; nothing ; number ; numbers ;
	ocean ; oceans ; of ; old ; older ; oldest ; on ; one ; or ; our ;
	people ; person ; persons ; place ; places ; population ; populations ; populous ;
	region ; regions ; rise ; risen ; rises ; river ; rivers ; rose ;
	s ; seamass ; seamasses ; sea ; seas ; seven ; she ; six ; small ; smaller ; smallest ; some ; somebody ; someone ; something ; south ; sqmile ; sqmiles ; sum ; sums ;
	ten ; than ; that ; the ; the(gram_number) ; their ; them ; there ; thing ; thousand ; three ; through ; time ; times ; to ; tomorrow ; total ; totals ; two ;
	upper_volta ; us ;
	was ; we ; were ; west ; what ; when ; where ; which ; who ; whom ; whose ; with ; 
	yesterday ; you ; your ; 	'@' ; '?' ; ',' ; '.' ; '!' ;
	pp(list(word_t), word_t) ; pp(word_t, unk_t3, alpha_t0, alpha_t0) ; np(beta_t0, kappa_t0, list(word_t)) ; np(word_t, beta_t0, alpha_t0, theta_t0, unk_t3, alpha_t0, alpha_t0) ;
	det(gamma_t0, gram_number, unk_t7) ; verb_form(word_t, delta_t0, beta_t0, unk_t10) ;
	neg(unk_t5, eta_t0) ; np_head0(kappa_t0, beta_t0, lambda_t0) ; verb(word_t, beta_t0, unk_t5, gram_voice) ; close ; predicate(eta_t0, word_t, alpha_t0) ;
	adv(word_t) ; gen_marker ; adv_phrase(word_t, alpha_t0, alpha_t0) ;
	proportion ; percentage ; arg(unk_t9, word_t) ; void ; verb(word_t, gram_voice, delta_t0, list(omicron_t0), eta_t0) ;
	conj(word_t, upsilon_t0) ; conj(word_t, word_t, word_t) ; reduced_rel(epsilon_t0, iota_t0) ; rel(epsilon_t0, iota_t0) ; there ; adj(word_t) ;
	to_word_t(word_t) ; comp(nu_t0, word_t, word_t) ; sup(xi_t0, word_t) ; value(word_t, chi_t0) ; part(gamma_t0, word_t).

:- type list(word_t) ---> x(unk_t4, phi_t0, word_t, list(word_t)) ; prep(eastof) ; prep(westof); prep(northof) ; prep(southof) ; poss.

:- type gram_number ---> sin ; plu.
:- type gram_gender ---> masc ; fem ; neut.
:- type unk_t3 ---> subj ; undef ; compl ; nil ; compl(any).
:- type unk_t4 ---> gap ; nogap.
:- type unk_t5 ---> trans ; ditrans ; intrans ; do ; be ; have ; unk_t10 + unk_t5.
:- type gram_voice ---> active ; passive.
:- type unk_t7 ---> def ; indef ; generic.
:- type unk_t8 ---> restr ; quant.
:- type unk_t9 ---> ind ; dir ; predicate.
:- type unk_t10 ---> main ; aux ; decl.

:- type alpha_t0 ---> #(integer, integer, integer) ; #(integer, integer, integer, integer).
:- type beta_t0 ---> integer+gram_number ; integer+beta_t0.
:- type gamma_t0 ---> int_det(epsilon_t0) ; quant(nu_t0, tau_t0) ; det(word_t) ; generic.
:- type gram_tense ---> pres ; past ; imp.
:- type part_fin ---> part ; fin.
:- type delta_t0 ---> inf ; gram_tense+part_fin.
:- type epsilon_t0.
:- type zeta_t0 ---> imp(iota_t0) ; q(iota_t0) ; whq(epsilon_t0, iota_t0) ; decl(iota_t0).
:- type eta_t0 ---> neg ; adj ; pos.
:- type theta_t0 ---> def ; indef.
:- type iota_t0 ---> s(word_t, word_t, list(word_t), list(word_t)).
:- type kappa_t0 ---> wh(epsilon_t0) ; np_head(word_t, list(word_t),word_t) ; np_head(gamma_t0, list(word_t),word_t) ; pronoun(gram_gender) ; name(word_t).
:- type lambda_t0 ---> def+common ; indef+common ; proper.
:- type mu_t0 ---> common ; proper.
:- type nu_t0 ---> 'the' ; 'same' ; not+less ; not+more ; less ; more.
:- type xi_t0 ---> 'least' ; 'most'.
:- type omicron_t0 ---> 'perf' ; 'prog'.
:- type pi_t0 ---> np.
:- type rho_t0 ---> prep(in) ; prep(at) ; to_rho_t0(list(word_t)).
:- type sigma_t0 ---> place ; time.
:- type tau_t0 ---> nb(integer) ; wh(epsilon_t0).
:- type upsilon_t0 ---> 'list' ; 'end'.
:- type phi_t0 ---> terminal ; nonterminal.
:- type chi_t0 ---> wh(epsilon_t0).


:- pred data(empty).
data(_).

:- pred benchmark(empty, empty).
benchmark(_Data, _Out) :-
    chat_parser.

:- pred chat_parser.
chat_parser :- 
    string(X),
    determinate_say(X,_),
    fail.
chat_parser.


%  query set

:- pred string(list(word_t)).
string([what,rivers,are,there,?]).
string([does,afghanistan,border,china,?]).
string([what,is,the,capital,of,upper_volta,?]).
string([where,is,the,largest,country,?]).
string([which,country,'@',s,capital,is,london,?]).
string([which,countries,are,european,?]).
string([how,large,is,the,smallest,american,country,?]).
string([what,is,the,ocean,that,borders,african,countries,and,that,borders,asian,countries,?]).
string([what,are,the,capitals,of,the,countries,bordering,the,baltic,?]).
string([which,countries,are,bordered,by,two,seas,?]).
string([how,many,countries,does,the,danube,flow,through,?]).
string([what,is,the,total,area,of,countries,south,of,the,equator,and,not,in,australasia,?]).
string([what,is,the,average,area,of,the,countries,in,each,continent,?]).
string([is,there,more,than,one,country,in,each,continent,?]).
string([is,there,some,ocean,that,does,not,border,any,country,?]).
string([what,are,the,countries,from,which,a,river,flows,into,the,black_sea,?]).


%  determinate_say

:- pred determinate_say(list(word_t), zeta_t0).
determinate_say(X,Y) :- 
   say(X,Y), !.


%-----------------------------------------------------------------------------
%
%  xgrun
%
%-----------------------------------------------------------------------------

:- pred terminal(word_t, list(word_t), list(word_t), list(word_t), list(word_t)).
terminal(T,S,S,x(_,terminal,T,X),X).
terminal(T,[T|S],S,X,X) :-
   gap(X).

:- pred gap(list(word_t)).
gap(x(gap,_,_,_)).
gap([]).

:- pred virtual(word_t, list(word_t), list(word_t)).
virtual(NT,x(_,nonterminal,NT,X),X).


%----------------------------------------------------------------------------
%
%  clotab
%
%----------------------------------------------------------------------------

% normal form masks

:- pred is_pp(alpha_t0).
is_pp(#(1,_,_,_)).

:- pred is_pred(alpha_t0).
is_pred(#(_,1,_,_)).

:- pred is_trace(alpha_t0).
is_trace(#(_,_,1,_)).

:- pred is_adv(alpha_t0).
is_adv(#(_,_,_,1)).

:- pred trace1(alpha_t0, alpha_t0).
trace1(#(_,_,1,_),#(0,0,0,0)).

:- pred trace1(alpha_t0).
trace1(#(0,0,1,0)).

:- pred adv(alpha_t0).
adv(#(0,0,0,1)).

:- pred empty(alpha_t0).
empty(#(0,0,0,0)).

:- pred np_all(alpha_t0).
np_all(#(1,1,1,0)).

:- pred s_all(alpha_t0).
s_all(#(1,0,1,1)).

:- pred np_no_trace(alpha_t0).
np_no_trace(#(1,1,0,0)).

% mask operations

:- pred myplus(alpha_t0, alpha_t0, alpha_t0).
myplus(#(B1,B2,B3,B4),#(C1,C2,C3,C4),#(D1,D2,D3,D4)) :-
   or(B1,C1,D1),
   or(B2,C2,D2),
   or(B3,C3,D3),
   or(B4,C4,D4).

:- pred minus(alpha_t0, alpha_t0, alpha_t0).
minus(#(B1,B2,B3,B4),#(C1,C2,C3,C4),#(D1,D2,D3,D4)) :-
   anot(B1,C1,D1),
   anot(B2,C2,D2),
   anot(B3,C3,D3),
   anot(B4,C4,D4).

:- pred or(integer, integer, integer).
or(1,_,1).
or(0,1,1).
or(0,0,0).

:- pred anot(integer, integer, integer).
anot(X,0,X).
anot(X,1,0).

% noun phrase position features

:- pred role(unk_t3, unk_t10, alpha_t0).
role(subj,_,#(1,0,0)).
role(compl,_,#(0,_,_)).
role(undef,main,#(_,0,_)).
role(undef,aux,#(0,_,_)).
role(undef,decl,_).
role(nil,_,_).

:- pred subj_case(alpha_t0).
subj_case(#(1,0,0)).
:- pred verb_case(alpha_t0).
verb_case(#(0,1,0)).
:- pred prep_case(alpha_t0).
prep_case(#(0,0,1)).
:- pred compl_case(alpha_t0).
compl_case(#(0,_,_)).


%----------------------------------------------------------------------------
%
%  newg
%
%----------------------------------------------------------------------------

:- pred say(list(word_t), zeta_t0).
say(X,Y) :-
   sentence(Y,X,[],[],[]).

:- pred sentence(zeta_t0, list(word_t), list(word_t), list(word_t), list(word_t)).
sentence(B,C,D,E,F) :-
   declarative(B,C,G,E,H),
   terminator(.,G,D,H,F).
sentence(B,C,D,E,F) :-
   wh_question(B,C,G,E,H),
   terminator(?,G,D,H,F).
sentence(B,C,D,E,F) :-
   topic(C,G,E,H),
   wh_question(B,G,I,H,J),
   terminator(?,I,D,J,F).
sentence(B,C,D,E,F) :-
   yn_question(B,C,G,E,H),
   terminator(?,G,D,H,F).
sentence(B,C,D,E,F) :-
   imperative(B,C,G,E,H),
   terminator(!,G,D,H,F).

:- pred pp(word_t, unk_t3, alpha_t0, alpha_t0, list(word_t), list(word_t), list(word_t), list(word_t)).
pp(B,C,D,E,F,F,G,H) :-
   virtual(pp(B,C,D,E),G,H).
pp(pp(B,C),D,E,F,G,H,I,J) :-
   prep(B,G,K,I,L),
   prep_case(M),
   np(C,N,M,O,D,E,F,K,H,L,J).

:- pred topic(list(word_t), list(word_t), list(word_t), list(word_t)).
topic(B,C,D,x(gap,nonterminal,pp(E,compl,F,G),H)) :-
   pp(E,compl,F,G,B,I,D,J),
   opt_comma(I,C,J,H).

:- pred opt_comma(list(word_t), list(word_t), list(word_t), list(word_t)).
opt_comma(B,C,D,E) :-
   @(',',B,C,D,E).
opt_comma(B,B,C,C).

:- pred declarative(zeta_t0, list(word_t), list(word_t), list(word_t), list(word_t)).
declarative(decl(B),C,D,E,F) :-
   s(B,G,C,D,E,F).

:- pred wh_question(zeta_t0, list(word_t), list(word_t), list(word_t), list(word_t)).
wh_question(whq(B,C),D,E,F,G) :-
   variable_q(B,H,I,J,D,K,F,L),
   question(I,J,C,K,E,L,G).

:- pred np(word_t, beta_t0, alpha_t0, theta_t0, unk_t3, alpha_t0, alpha_t0, list(word_t), list(word_t), list(word_t), list(word_t)).
np(B,C,D,E,F,G,H,I,I,J,K) :-
   virtual(np(B,C,D,E,F,G,H),J,K).
np(to_word_t(np(B,C,[])),B,D,def,E,F,G,H,I,J,K) :-
   is_pp(F),
   pers_pron(C,B,L,H,I,J,K),
   empty(G),
   role(L,decl,D).
np(to_word_t(np(B,C,D)),B,E,F,G,H,I,J,K,L,M) :-
   is_pp(H),
   np_head(C,B,F+N,O,D,J,P,L,Q),
   np_all(R),
   np_compls(N,B,G,O,R,I,P,K,Q,M).
np(part(B,C),3+D,E,indef,F,G,H,I,J,K,L) :-
   is_pp(G),
   determiner(B,D,indef,I,M,K,N),
   @(of,M,O,N,P),
   s_all(Q),
   prep_case(R),
   np(C,3+plu,R,def,F,Q,H,O,J,P,L).

:- pred variable_q(epsilon_t0, beta_t0, unk_t3, alpha_t0, list(word_t), list(word_t), list(word_t), list(word_t)).
variable_q(B,C,D,E,F,G,H,x(gap,nonterminal,np(I,C,E,J,K,L,M),N)) :-
   whq(B,C,I,D,F,G,H,N),
   trace1(L,M).
variable_q(B,C,compl,D,E,F,G,x(gap,nonterminal,pp(pp(H,I),compl,J,K),L)) :-
   prep(H,E,M,G,N),
   whq(B,C,I,O,M,F,N,L),
   trace1(J,K),
   compl_case(D).
variable_q(B,C,compl,D,E,F,G,x(gap,nonterminal,
	   adv_phrase(pp(H,np(C,np_head(int_det(B),[],I),[])),J,K),L)) :-
   context_pron(to_rho_t0(H),I,E,F,G,L),
   trace1(J,K),
   verb_case(D).
variable_q(B,C,compl,D,E,F,G,
	   x(gap,nonterminal,predicate(adj,value(H,wh(B)),I),J)) :-
   @(how,E,K,G,L),
   adj(quant,H,K,F,L,J),
   empty(I),
   verb_case(D).

:- pred adv_phrase(word_t, alpha_t0, alpha_t0, list(word_t), list(word_t), list(word_t), list(word_t)).
adv_phrase(B,C,D,E,E,F,G) :-
   virtual(adv_phrase(B,C,D),F,G).
adv_phrase(pp(B,C),D,E,F,G,H,I) :-
   loc_pred(B,F,J,H,K),
   pp(pp(prep(of),C),compl,D,E,J,G,K,I).

:- pred predicate(eta_t0, word_t, alpha_t0, list(word_t), list(word_t), list(word_t), list(word_t)).
predicate(B,C,D,E,E,F,G) :-
   virtual(predicate(B,C,D),F,G).
predicate(B,C,D,E,F,G,H) :-
   adj_phrase(C,D,E,F,G,H).
predicate(neg,B,C,D,E,F,G) :-
   s_all(H),
   pp(B,compl,H,C,D,E,F,G).
predicate(B,C,D,E,F,G,H) :-
   s_all(I),
   adv_phrase(C,I,D,E,F,G,H).

:- pred whq(epsilon_t0, beta_t0, word_t, unk_t3, list(word_t), list(word_t), list(word_t), list(word_t)).
whq(B,C,D,undef,E,F,G,H) :-
   int_det(B,C,E,I,G,J),
   s_all(K),
   np(to_word_t(D),C,L,M,subj,K,N,I,F,J,H).
whq(B,3+C,np(3+C,wh(B),[]),D,E,F,G,H) :-
   int_pron(D,E,F,G,H).

:- pred int_det(epsilon_t0, beta_t0, list(word_t), list(word_t), list(word_t), list(word_t)).
int_det(B,3+C,D,E,F,G) :-
   whose(B,C,D,E,F,G).
int_det(B,3+C,D,E,F,G) :-
   int_art(B,C,D,E,F,G).

:- pred gen_marker(list(word_t), list(word_t), list(word_t), list(word_t)).
gen_marker(B,B,C,D) :-
   virtual(gen_marker,C,D).
gen_marker(B,C,D,E) :-
   @('@',B,F,D,G),
   an_s(F,C,G,E).

:- pred whose(epsilon_t0, beta_t0, list(word_t), list(word_t), list(word_t), list(word_t)).
whose(B,C,D,E,F,x(nogap,nonterminal,np_head0(wh(B),C,proper),
      x(nogap,nonterminal,gen_marker,G))) :-
   @(whose,D,E,F,G).

:- pred question(unk_t3, alpha_t0, iota_t0, list(word_t), list(word_t), list(word_t), list(word_t)).
question(B,C,D,E,F,G,H) :-
   subj_question(B),
   role(subj,I,C),
   s(D,J,E,F,G,H).
question(B,C,D,E,F,G,H) :-
   fronted_verb(B,C,E,I,G,J),
   s(D,K,I,F,J,H).

:- pred det(gamma_t0, gram_number, unk_t7, list(word_t), list(word_t), list(word_t), list(word_t)).
det(B,C,D,E,E,F,G) :-
   virtual(det(B,C,D),F,G).
det(det(B),C,D,E,F,G,H) :-
   terminal(I,E,F,G,H),
   det(I,C,B,D).
det(generic,B,generic,C,C,D,D).

:- pred int_art(epsilon_t0, gram_number, list(word_t), list(word_t), list(word_t), list(word_t)).
int_art(B,C,D,E,F,x(nogap,nonterminal,det(G,C,def),H)) :-
   int_art(B,C,G,D,E,F,H).

:- pred subj_question(unk_t3).
subj_question(subj).
subj_question(undef).

:- pred yn_question(zeta_t0, list(word_t), list(word_t), list(word_t), list(word_t)).
yn_question(q(B),C,D,E,F) :-
   fronted_verb(nil,G,C,H,E,I),
   s(B,J,H,D,I,F).

:- pred verb_form(word_t, delta_t0, beta_t0, unk_t10, list(word_t), list(word_t), list(word_t), list(word_t)).
verb_form(B,C,D,E,F,F,G,H) :-
   virtual(verb_form(B,C,D,E),G,H).
verb_form(B,C,D,E,F,G,H,I) :-
   terminal(J,F,G,H,I),
   verb_form(J,B,C,D).

:- pred neg(unk_t5, eta_t0, list(word_t), list(word_t), list(word_t), list(word_t)).
neg(B,C,D,D,E,F) :-
   virtual(neg(B,C),E,F).
neg(aux+B,neg,C,D,E,F) :-
   @(not,C,D,E,F).
neg(B,pos,C,C,D,D).

:- pred fronted_verb(unk_t3, alpha_t0, list(word_t), list(word_t), list(word_t), list(word_t)).
fronted_verb(B,C,D,E,F,x(gap,nonterminal,verb_form(G,H,I,J),
	     x(nogap,nonterminal,neg(K,L),M))) :-
   verb_form(G,H,I,N,D,O,F,P),
   verb_type(G,aux+Q),
   role(B,J,C),
   neg(R,L,O,E,P,M).

:- pred imperative(zeta_t0, list(word_t), list(word_t), list(word_t), list(word_t)).
imperative(imp(B),C,D,E,F) :-
   imperative_verb(C,G,E,H),
   s(B,I,G,D,H,F).

:- pred imperative_verb(list(word_t), list(word_t), list(word_t), list(word_t)).
imperative_verb(B,C,D,x(nogap,terminal,you,x(nogap,nonterminal,
		verb_form(E,imp+fin,2+sin,main),F))) :-
   verb_form(E,inf,G,H,B,C,D,F).

:- pred s(iota_t0, alpha_t0, list(word_t), list(word_t), list(word_t), list(word_t)).
s(s(B,C,D,E),F,G,H,I,J) :-
   subj(B,K,L,G,M,I,N),
   verb(C,K,L,O,M,P,N,Q),
   empty(R),
   s_all(S),
   verb_args(L,O,D,R,T,P,U,Q,V),
   minus(S,T,W),
   myplus(S,T,X),
   verb_mods(E,W,X,F,U,H,V,J).

:- pred subj(word_t, beta_t0, unk_t5, list(word_t), list(word_t), list(word_t), list(word_t)).
subj(there,B,C+be,D,E,F,G) :-
   @(there,D,E,F,G).
subj(B,C,D,E,F,G,H) :-
   s_all(I),
   subj_case(J),
   np(B,C,J,K,subj,I,L,E,F,G,H).

:- pred np_head(kappa_t0, beta_t0, lambda_t0, list(word_t), list(word_t), list(word_t), list(word_t), list(word_t), list(word_t)).
np_head(B,C,D,E,F,G,H,I,J) :-
   np_head0(K,L,M,G,N,I,O),
   possessive(K,L,M,P,P,B,C,D,E,F,N,H,O,J).

:- pred np_head0(kappa_t0, beta_t0, lambda_t0, list(word_t), list(word_t), list(word_t), list(word_t)).
np_head0(B,C,D,E,E,F,G) :-
   virtual(np_head0(B,C,D),F,G).
np_head0(name(B),3+sin,def+proper,C,D,E,F) :-
   name(B,C,D,E,F).
np_head0(np_head(B,C,D),3+E,F+common,G,H,I,J) :-
   determiner(B,E,F,G,K,I,L),
   adjs(C,K,M,L,N),
   noun(D,E,M,H,N,J).
np_head0(B,C,def+proper,D,E,F,x(nogap,nonterminal,gen_marker,G)) :-
   poss_pron(B,C,D,E,F,G).
np_head0(np_head(B,[],C),3+sin,indef+common,D,E,F,G) :-
   quantifier_pron(B,C,D,E,F,G).

:- pred np_compls(mu_t0, beta_t0, unk_t3, list(word_t), alpha_t0, alpha_t0, list(word_t), list(word_t), list(word_t), list(word_t)).
np_compls(proper,B,C,[],D,E,F,F,G,G) :-
   empty(E).
np_compls(common,B,C,D,E,F,G,H,I,J) :-
   np_all(K),
   np_mods(B,C,L,D,E,M,K,N,G,O,I,P),
   relative(B,L,M,N,F,O,H,P,J).

:- pred possessive(kappa_t0, beta_t0, lambda_t0, list(word_t), list(word_t), kappa_t0, beta_t0, lambda_t0, list(word_t), list(word_t), list(word_t), list(word_t), list(word_t), list(word_t)).
possessive(B,C,D,[],E,F,G,H,I,J,K,L,M,N) :-
   gen_case(K,O,M,P),
   np_head0(Q,R,S,O,T,P,U),
   possessive(Q,R,S,V,[pp(poss,to_word_t(np(C,B,E)))|V],F,G,H,I,J,T,L,U,N).
possessive(B,C,D,E,F,B,C,D,E,F,G,G,H,H).

:- pred gen_case(list(word_t), list(word_t), list(word_t), list(word_t)).
gen_case(B,C,D,x(nogap,terminal,the,E)) :-
   gen_marker(B,C,D,E).

:- pred an_s(list(word_t), list(word_t), list(word_t), list(word_t)).
an_s(B,C,D,E) :-
   @(s,B,C,D,E).
an_s(B,B,C,C).

:- pred determiner(gamma_t0, gram_number, unk_t7, list(word_t), list(word_t), list(word_t), list(word_t)).
determiner(B,C,D,E,F,G,H) :-
   det(B,C,D,E,F,G,H).
determiner(B,C,D,E,F,G,H) :-
   quant_phrase(B,C,D,E,F,G,H).

:- pred quant_phrase(gamma_t0, gram_number, unk_t7, list(word_t), list(word_t), list(word_t), list(word_t)).
quant_phrase(quant(B,C),D,E,F,G,H,I) :-
   quant(B,E,F,J,H,K),
   number(C,D,J,G,K,I).

:- pred quant(nu_t0, unk_t7, list(word_t), list(word_t), list(word_t), list(word_t)).
quant(B,indef,C,D,E,F) :-
   neg_adv(G,B,C,H,E,I),
   comp_adv(G,H,J,I,K),
   @(than,J,D,K,F).
quant(B,indef,C,D,E,F) :-
   @(at,C,G,E,H),
   sup_adv(I,G,D,H,F),
   sup_op(I,B).
quant(the,def,B,C,D,E) :-
   @(the,B,C,D,E).
quant(same,indef,B,B,C,C).

:- pred neg_adv(nu_t0, nu_t0, list(word_t), list(word_t), list(word_t), list(word_t)).
neg_adv(B,not+B,C,D,E,F) :-
   @(not,C,D,E,F).
neg_adv(B,B,C,C,D,D).

:- pred sup_op(xi_t0, nu_t0).
sup_op(least,not+less).
sup_op(most,not+more).

:- pred np_mods(beta_t0, unk_t3, list(word_t), list(word_t), alpha_t0, alpha_t0, alpha_t0, alpha_t0, list(word_t), list(word_t), list(word_t), list(word_t)).
np_mods(B,C,D,[E|F],G,H,I,J,K,L,M,N) :-
   np_mod(B,C,E,G,O,K,P,M,Q),
   trace1(R),
   myplus(R,O,S),
   minus(G,S,T),
   myplus(O,G,U),
   np_mods(B,C,D,F,T,H,U,J,P,L,Q,N).
np_mods(B,C,D,D,E,E,F,F,G,G,H,H).

:- pred np_mod(beta_t0, unk_t3, word_t, alpha_t0, alpha_t0, list(word_t), list(word_t), list(word_t), list(word_t)).
np_mod(B,C,D,E,F,G,H,I,J) :-
   pp(D,C,E,F,G,H,I,J).
np_mod(B,C,D,E,F,G,H,I,J) :-
   reduced_relative(B,to_word_t(D),E,F,G,H,I,J).

:- pred verb_mods(list(word_t), alpha_t0, alpha_t0, alpha_t0, list(word_t), list(word_t), list(word_t), list(word_t)).
verb_mods([B|C],D,E,F,G,H,I,J) :-
   verb_mod(B,D,K,G,L,I,M),
   trace1(N),
   myplus(N,K,O),
   minus(D,O,P),
   myplus(K,D,Q),
   verb_mods(C,P,Q,F,L,H,M,J).
verb_mods([],B,C,C,D,D,E,E).

:- pred verb_mod(word_t, alpha_t0, alpha_t0, list(word_t), list(word_t), list(word_t), list(word_t)).
verb_mod(B,C,D,E,F,G,H) :-
   adv_phrase(B,C,D,E,F,G,H).
verb_mod(B,C,D,E,F,G,H) :-
   is_adv(C),
   adverb(B,E,F,G,H),
   empty(D).
verb_mod(B,C,D,E,F,G,H) :-
   pp(B,compl,C,D,E,F,G,H).

:- pred adjs(list(word_t), list(word_t), list(word_t), list(word_t), list(word_t)).
adjs([B|C],D,E,F,G) :-
   pre_adj(B,D,H,F,I),
   adjs(C,H,E,I,G).
adjs([],B,B,C,C).


:- pred pre_adj(word_t, list(word_t), list(word_t), list(word_t), list(word_t)).
pre_adj(B,C,D,E,F) :-
   adj(G,B,C,D,E,F).
pre_adj(B,C,D,E,F) :-
   sup_phrase(B,C,D,E,F).

:- pred sup_phrase(word_t, list(word_t), list(word_t), list(word_t), list(word_t)).
sup_phrase(sup(most,B),C,D,E,F) :-
   sup_adj(B,C,D,E,F).
sup_phrase(sup(B,C),D,E,F,G) :-
   sup_adv(B,D,I,F,J),
   adj(quant,C,I,E,J,G).

:- pred comp_phrase(word_t, alpha_t0, list(word_t), list(word_t), list(word_t), list(word_t)).
comp_phrase(comp(B,C,D),E,F,G,H,I) :-
   comp(B,C,F,J,H,K),
   np_no_trace(L),
   prep_case(M),
   np(D,N,M,O,compl,L,E,J,G,K,I).

:- pred comp(nu_t0, word_t, list(word_t), list(word_t), list(word_t), list(word_t)).
comp(B,C,D,E,F,G) :-
   comp_adv(B,D,H,F,I),
   adj(quant,C,H,J,I,K),
   @(than,J,E,K,G).
comp(more,B,C,D,E,F) :-
   rel_adj(B,C,G,E,H),
   @(than,G,D,H,F).
comp(same,B,C,D,E,F) :-
   @(as,C,G,E,H),
   adj(quant,B,G,I,H,J),
   @(as,I,D,J,F).

:- pred relative(beta_t0, list(word_t), alpha_t0, alpha_t0, alpha_t0, list(word_t), list(word_t), list(word_t), list(word_t)).
relative(B,[C],D,E,F,G,H,I,J) :-
   is_pred(D),
   rel_conj(B,K,to_word_t(C),F,G,H,I,J).
relative(B,[],C,D,D,E,E,F,F).

:- pred rel_conj(beta_t0, word_t, word_t, alpha_t0, list(word_t), list(word_t), list(word_t), list(word_t)).
rel_conj(B,C,D,E,F,G,H,I) :-
   rel(B,J,K,F,L,H,M),
   rel_rest(B,C,J,D,K,E,L,G,M,I).

:- pred rel_rest(beta_t0, word_t, word_t, word_t, alpha_t0, alpha_t0, list(word_t), list(word_t), list(word_t), list(word_t)).
rel_rest(B,C,D,E,F,G,H,I,J,K) :-
   conj(C,L,D,M,E,H,N,J,O),
   rel_conj(B,L,M,G,N,I,O,K).
rel_rest(B,C,D,D,E,E,F,F,G,G).

:- pred rel(beta_t0, word_t, alpha_t0, list(word_t), list(word_t), list(word_t), list(word_t)).
rel(B,rel(C,D),E,F,G,H,I) :-
   myopen(F,J,H,K),
   variable(B,C,J,L,K,M),
   s(D,N,L,O,M,P),
   trace1(Q),
   minus(N,Q,E),
   close(O,G,P,I).

:- pred variable(beta_t0, epsilon_t0, list(word_t), list(word_t), list(word_t), list(word_t)).
variable(B,C,D,E,F,x(gap,nonterminal,np(np(B,wh(C),[]),B,G,H,I,J,K),L)) :-
   @(that,D,E,F,L),
   trace1(J,K).
variable(B,C,D,E,F,x(gap,nonterminal,np(G,H,I,J,K,L,M),N)) :-
   wh(C,B,to_word_t(G),H,I,D,E,F,N),
   trace1(L,M).
variable(B,C,D,E,F,x(gap,nonterminal,pp(pp(G,H),compl,I,J),K)) :-
   prep(G,D,L,F,M),
   wh(C,B,H,N,O,L,E,M,K),
   trace1(I,J),
   compl_case(O).

:- pred wh(epsilon_t0, beta_t0, word_t, beta_t0, alpha_t0, list(word_t), list(word_t), list(word_t), list(word_t)).
wh(B,C,to_word_t(np(C,wh(B),[])),C,D,E,F,G,H) :-
   rel_pron(I,E,F,G,H),
   role(I,decl,D).
wh(B,C,to_word_t(np(D,E,[pp(F,G)])),D,H,I,J,K,L) :-
   np_head0(E,D,M+common,I,N,K,O),
   prep(F,N,P,O,Q),
   wh(B,C,G,R,S,P,J,Q,L).
wh(B,C,D,E,F,G,H,I,J) :-
   whose(B,C,G,K,I,L),
   s_all(M),
   np(D,E,F,def,subj,M,N,K,H,L,J).

:- pred reduced_relative(beta_t0, word_t, alpha_t0, alpha_t0, list(word_t), list(word_t), list(word_t), list(word_t)).
reduced_relative(B,C,D,E,F,G,H,I) :-
   is_pred(D),
   reduced_rel_conj(B,J,C,E,F,G,H,I).

:- pred reduced_rel_conj(beta_t0, word_t, word_t, alpha_t0, list(word_t), list(word_t), list(word_t), list(word_t)).
reduced_rel_conj(B,C,D,E,F,G,H,I) :-
   reduced_rel(B,J,K,F,L,H,M),
   reduced_rel_rest(B,C,J,D,K,E,L,G,M,I).

:- pred reduced_rel_rest(beta_t0, word_t, word_t, word_t, alpha_t0, alpha_t0, list(word_t), list(word_t), list(word_t), list(word_t)).
reduced_rel_rest(B,C,D,E,F,G,H,I,J,K) :-
   conj(C,L,D,M,E,H,N,J,O),
   reduced_rel_conj(B,L,M,G,N,I,O,K).
reduced_rel_rest(B,C,D,D,E,E,F,F,G,G).

:- pred reduced_rel(beta_t0, word_t, alpha_t0, list(word_t), list(word_t), list(word_t), list(word_t)).
reduced_rel(B,reduced_rel(C,D),E,F,G,H,I) :-
   myopen(F,J,H,K),
   reduced_wh(B,C,J,L,K,M),
   s(D,N,L,O,M,P),
   trace1(Q),
   minus(N,Q,E),
   close(O,G,P,I).

:- pred reduced_wh(beta_t0, epsilon_t0, list(word_t), list(word_t), list(word_t), list(word_t)).
reduced_wh(B,C,D,E,F,x(nogap,nonterminal,
	   np(np(B,wh(C),[]),B,G,H,I,J,K),x(nogap,nonterminal,
	   verb_form(be,pres+fin,B,main),x(nogap,nonterminal,
	   neg(L,M),x(nogap,nonterminal,predicate(M,N,O),P))))) :-
   neg(Q,M,D,R,F,S),
   predicate(M,N,O,R,E,S,P),
   trace1(J,K),
   subj_case(G).
reduced_wh(B,C,D,E,F,x(nogap,nonterminal,
	   np(np(B,wh(C),[]),B,G,H,I,J,K),x(nogap,nonterminal,
	   verb(L,M,N,O),P))) :-
   participle(L,N,O,D,E,F,P),
   trace1(J,K),
   subj_case(G).
reduced_wh(B,C,D,E,F,x(nogap,nonterminal,
	   np(G,H,I,J,K,L,M),x(gap,nonterminal,
	   np(np(B,wh(C),[]),B,N,O,P,Q,R),S))) :-
   s_all(T),
   subj_case(I),
   verb_case(N),
   np(to_word_t(G),H,U,J,subj,T,V,D,E,F,S),
   trace1(L,M),
   trace1(Q,R).

:- pred verb(word_t, beta_t0, unk_t5, gram_voice, list(word_t), list(word_t), list(word_t), list(word_t)).
verb(B,C,D,E,F,F,G,H) :-
   virtual(verb(B,C,D,E),G,H).
verb(verb(B,C,D+fin,E,F),G,H,C,I,J,K,L) :-
   verb_form(M,D+fin,G,N,I,O,K,P),
   verb_type(M,Q),
   neg(Q,F,O,R,P,S),
   rest_verb(N,M,B,C,E,R,J,S,L),
   verb_type(B,H).

:- pred rest_verb(unk_t10, word_t, word_t, gram_voice, list(omicron_t0), list(word_t), list(word_t), list(word_t), list(word_t)).
rest_verb(aux,have,B,C,[perf|D],E,F,G,H) :-
   verb_form(I,past+part,J,K,E,L,G,M),
   have(I,B,C,D,L,F,M,H).
rest_verb(aux,be,B,C,D,E,F,G,H) :-
   verb_form(I,J,K,L,E,M,G,N),
   be(J,I,B,C,D,M,F,N,H).
rest_verb(aux,do,B,active,[],C,D,E,F) :-
   verb_form(B,inf,G,H,C,D,E,F).
rest_verb(main,B,B,active,[],C,C,D,D).

:- pred have(word_t, word_t, gram_voice, list(omicron_t0), list(word_t), list(word_t), list(word_t), list(word_t)).
have(be,B,C,D,E,F,G,H) :-
   verb_form(I,J,K,L,E,M,G,N),
   be(J,I,B,C,D,M,F,N,H).
have(B,B,active,[],C,C,D,D).

:- pred be(delta_t0, word_t, word_t, gram_voice, list(omicron_t0), list(word_t), list(word_t), list(word_t), list(word_t)).
be(past+part,B,B,passive,[],C,C,D,D).
be(pres+part,B,C,D,[prog],E,F,G,H) :-
   passive(B,C,D,E,F,G,H).

:- pred passive(word_t, word_t, gram_voice, list(word_t), list(word_t), list(word_t), list(word_t)).
passive(be,B,passive,C,D,E,F) :-
   verb_form(B,past+part,G,H,C,D,E,F),
   verb_type(B,I),
   passive(I).
passive(B,B,active,C,C,D,D).

:- pred participle(word_t, unk_t5, gram_voice, list(word_t), list(word_t), list(word_t), list(word_t)).
participle(verb(B,C,inf,D,E),F,C,G,H,I,J) :-
   neg(K,E,G,L,I,M),
   verb_form(B,N,O,P,L,H,M,J),
   participle(N,C,D),
   verb_type(B,F).

:- pred passive(unk_t5).
passive(B+trans).
passive(B+ditrans).

:- pred participle(delta_t0, gram_voice, list(omicron_t0)).
participle(pres+part,active,[prog]).
participle(past+part,passive,[]).

:- pred close(list(word_t), list(word_t), list(word_t), list(word_t)).
close(B,B,C,D) :-
   virtual(close,C,D).

:- pred myopen(list(word_t), list(word_t), list(word_t), list(word_t)).
myopen(B,B,C,x(gap,nonterminal,close,C)).

:- pred verb_args(unk_t5, gram_voice, list(word_t), alpha_t0, alpha_t0, list(word_t), list(word_t), list(word_t), list(word_t)).
verb_args(B+C,D,E,F,G,H,I,J,K) :-
   advs(E,L,M,H,N,J,O),
   verb_args(C,D,L,F,G,N,I,O,K).
verb_args(trans,active,[arg(dir,B)],C,D,E,F,G,H) :-
   verb_arg(np,B,D,E,F,G,H).
verb_args(ditrans,B,[arg(C,D)|E],F,G,H,I,J,K) :-
   verb_arg(np,D,L,H,M,J,N),
   object(C,E,L,G,M,I,N,K).
verb_args(be,B,[void],C,C,D,E,F,G) :-
   terminal(there,D,E,F,G).
verb_args(be,B,[arg(predicate,C)],D,E,F,G,H,I) :-
   pred_conj(J,C,E,F,G,H,I).
verb_args(be,B,[arg(dir,C)],D,E,F,G,H,I) :-
   verb_arg(np,C,E,F,G,H,I).
verb_args(have,active,[arg(dir,B)],C,D,E,F,G,H) :-
   verb_arg(np,B,D,E,F,G,H).
verb_args(B,C,[],D,D,E,E,F,F) :-
   no_args(B).

:- pred object(unk_t9, list(word_t), alpha_t0, alpha_t0, list(word_t), list(word_t), list(word_t), list(word_t)).
object(B,C,D,E,F,G,H,I) :-
   adv(J),
   minus(J,D,K),
   advs(C,L,K,F,M,H,N),
   obj(B,L,D,E,M,G,N,I).

:- pred obj(unk_t9, list(word_t), alpha_t0, alpha_t0, list(word_t), list(word_t), list(word_t), list(word_t)).
obj(ind,[arg(dir,B)],C,D,E,F,G,H) :-
   verb_arg(np,B,D,E,F,G,H).
obj(dir,[],B,B,C,C,D,D).

:- pred pred_conj(word_t, word_t, alpha_t0, list(word_t), list(word_t), list(word_t), list(word_t)).
pred_conj(B,C,D,E,F,G,H) :-
   predicate(I,J,K,E,L,G,M),
   pred_rest(B,J,C,K,D,L,F,M,H).

:- pred pred_rest(word_t, word_t, word_t, alpha_t0, alpha_t0, list(word_t), list(word_t), list(word_t), list(word_t)).
pred_rest(B,C,D,E,F,G,H,I,J) :-
   conj(B,K,C,L,D,G,M,I,N),
   pred_conj(K,L,F,M,H,N,J).
pred_rest(B,C,C,D,D,E,E,F,F).

:- pred verb_arg(pi_t0, word_t, alpha_t0, list(word_t), list(word_t), list(word_t), list(word_t)).
verb_arg(np,B,C,D,E,F,G) :-
   s_all(H),
   verb_case(I),
   np(B,J,I,K,compl,H,C,D,E,F,G).

:- pred advs(list(word_t), list(word_t), alpha_t0, list(word_t), list(word_t), list(word_t), list(word_t)).
advs([B|C],D,E,F,G,H,I) :-
   is_adv(E),
   adverb(B,F,J,H,K),
   advs(C,D,E,J,G,K,I).
advs(B,B,C,D,D,E,E).

:- pred adj_phrase(word_t, alpha_t0, list(word_t), list(word_t), list(word_t), list(word_t)).
adj_phrase(B,C,D,E,F,G) :-
   adj(H,B,D,E,F,G),
   empty(C).
adj_phrase(B,C,D,E,F,G) :-
   comp_phrase(B,C,D,E,F,G).

:- pred no_args(unk_t5).
no_args(trans).
no_args(ditrans).
no_args(intrans).

:- pred conj(word_t, word_t, word_t, word_t, word_t, list(word_t), list(word_t), list(word_t), list(word_t)).
conj(conj(B,C),conj(B,D),E,F,conj(B,E,F),G,H,I,J) :-
   conj(B,C,D,G,H,I,J).

:- pred noun(word_t, gram_number, list(word_t), list(word_t), list(word_t), list(word_t)).
noun(B,C,D,E,F,G) :-
   terminal(H,D,E,F,G),
   noun_form(H,B,C).

:- pred adj(unk_t8, word_t, list(word_t), list(word_t), list(word_t), list(word_t)).
adj(B,adj(C),D,E,F,G) :-
   terminal(C,D,E,F,G),
   adj(C,B).

:- pred prep(list(word_t), list(word_t), list(word_t), list(word_t), list(word_t)).
prep(prep(B),C,D,E,F) :-
   terminal(B,C,D,E,F),
   prep(B).

:- pred rel_adj(word_t, list(word_t), list(word_t), list(word_t), list(word_t)).
rel_adj(adj(B),C,D,E,F) :-
   terminal(G,C,D,E,F),
   rel_adj(G,B).

:- pred sup_adj(word_t, list(word_t), list(word_t), list(word_t), list(word_t)).
sup_adj(adj(B),C,D,E,F) :-
   terminal(G,C,D,E,F),
   sup_adj(G,B).

:- pred comp_adv(nu_t0, list(word_t), list(word_t), list(word_t), list(word_t)).
comp_adv(less,B,C,D,E) :-
   @(less,B,C,D,E).
comp_adv(more,B,C,D,E) :-
   @(more,B,C,D,E).

:- pred sup_adv(xi_t0, list(word_t), list(word_t), list(word_t), list(word_t)).
sup_adv(least,B,C,D,E) :-
   @(least,B,C,D,E).
sup_adv(most,B,C,D,E) :-
   @(most,B,C,D,E).

:- pred rel_pron(unk_t3, list(word_t), list(word_t), list(word_t), list(word_t)).
rel_pron(B,C,D,E,F) :-
   terminal(G,C,D,E,F),
   rel_pron(G,B).

:- pred name(word_t, list(word_t), list(word_t), list(word_t), list(word_t)).
name(B,C,D,E,F) :-
   opt_the(C,G,E,H),
   terminal(B,G,D,H,F),
   name(B).

:- pred int_art(epsilon_t0, gram_number, gamma_t0, list(word_t), list(word_t), list(word_t), list(word_t)).
int_art(B,plu,quant(same,wh(B)),C,D,E,F) :-
   @(how,C,G,E,H),
   @(many,G,D,H,F).
int_art(B,C,D,E,F,G,H) :-
   terminal(I,E,F,G,H),
   int_art(I,B,C,D).

:- pred int_pron(unk_t3, list(word_t), list(word_t), list(word_t), list(word_t)).
int_pron(B,C,D,E,F) :-
   terminal(G,C,D,E,F),
   int_pron(G,B).

:- pred adverb(word_t, list(word_t), list(word_t), list(word_t), list(word_t)).
adverb(adv(B),C,D,E,F) :-
   terminal(B,C,D,E,F),
   adverb(B).

:- pred poss_pron(kappa_t0, beta_t0, list(word_t), list(word_t), list(word_t), list(word_t)).
poss_pron(pronoun(B),C+D,E,F,G,H) :-
   terminal(I,E,F,G,H),
   poss_pron(I,B,C,D).

:- pred pers_pron(kappa_t0, beta_t0, unk_t3, list(word_t), list(word_t), list(word_t), list(word_t)).
pers_pron(pronoun(B),C+D,E,F,G,H,I) :-
   terminal(J,F,G,H,I),
   pers_pron(J,B,C,D,E).

:- pred quantifier_pron(word_t, word_t, list(word_t), list(word_t), list(word_t), list(word_t)).
quantifier_pron(B,C,D,E,F,G) :-
   terminal(H,D,E,F,G),
   quantifier_pron(H,B,C).

:- pred context_pron(rho_t0, sigma_t0, list(word_t), list(word_t), list(word_t), list(word_t)).
context_pron(prep(in),place,B,C,D,E) :-
   @(where,B,C,D,E).
context_pron(prep(at),time,B,C,D,E) :-
   @(when,B,C,D,E).

:- pred number(tau_t0, gram_number, list(word_t), list(word_t), list(word_t), list(word_t)).
number(nb(B),C,D,E,F,G) :-
   terminal(H,D,E,F,G),
   number(H,B,C).

:- pred terminator(word_t, list(word_t), list(word_t), list(word_t), list(word_t)).
terminator(B,C,D,E,F) :-
   terminal(G,C,D,E,F),
   terminator(G,B).

:- pred opt_the(list(word_t), list(word_t), list(word_t), list(word_t)).
opt_the(B,B,C,C).
opt_the(B,C,D,E) :-
   @(the,B,C,D,E).

:- pred conj(word_t, upsilon_t0, upsilon_t0, list(word_t), list(word_t), list(word_t), list(word_t)).
conj(B,list,list,C,D,E,F) :-
   terminal(',',C,D,E,F).
conj(B,list,'end',C,D,E,F) :-
   terminal(B,C,D,E,F),
   conj(B).

:- pred loc_pred(list(word_t), list(word_t), list(word_t), list(word_t), list(word_t)).
loc_pred(B,C,D,E,F) :-
   terminal(G,C,D,E,F),
   loc_pred(G,B).

:- pred @(word_t, list(word_t), list(word_t), list(word_t), list(word_t)).
@(B,C,D,E,F) :-
   terminal(B,C,D,E,F),
   @(B).


%----------------------------------------------------------------------------
%
%  newdic
%
%----------------------------------------------------------------------------

:- pred word(word_t).
word(Word) :- @(Word).
word(Word) :- conj(Word).
word(Word) :- adverb(Word).
word(Word) :- sup_adj(Word,_).
word(Word) :- rel_adj(Word,_).
word(Word) :- adj(Word,_).
word(Word) :- name(Word).
word(Word) :- terminator(Word,_).
word(Word) :- pers_pron(Word,_,_,_,_).
word(Word) :- poss_pron(Word,_,_,_).
word(Word) :- rel_pron(Word,_).
word(Word) :- verb_form(Word,_,_,_).
word(Word) :- noun_form(Word,_,_).
word(Word) :- prep(Word).
word(Word) :- quantifier_pron(Word,_,_).
word(Word) :- number(Word,_,_).
word(Word) :- det(Word,_,_,_).
word(Word) :- int_art(Word,_,_,_).
word(Word) :- int_pron(Word,_).
word(Word) :- loc_pred(Word,_).

:- pred @(word_t).
@(how).
@(whose).
@(there).
@(of).
@('@').		% use @ instead of ' to help assembler
@(',').	
@(s).
@(than).
@(at).
@(the).
@(not).
@(as).
@(that).
@(less).
@(more).
@(least).
@(most).
@(many).
@(where).
@(when).

:- pred conj(word_t).
conj(and).
conj(or).

:- pred int_pron(word_t, unk_t3).
int_pron(what,undef).
int_pron(which,undef).
int_pron(who,subj).
int_pron(whom,compl).

:- pred int_art(word_t, epsilon_t0, gram_number, gamma_t0).
int_art(what,X,_,int_det(X)).
int_art(which,X,_,int_det(X)).

:- pred det(word_t, gram_number, word_t, unk_t7).
det(the,No,the(No),def).
det(a,sin,a,indef).
det(an,sin,a,indef).
det(every,sin,every,indef).
det(some,_,some,indef).
det(any,_,any,indef).
det(all,plu,all,indef).
det(each,sin,each,indef).
det(no,_,no,indef).

:- pred number(word_t, integer, gram_number).
number(W,I,Nb) :-
   tr_number(W,I),
   ag_number(I,Nb).

:- pred tr_number(word_t, integer).
tr_number(nb(I),I).
tr_number(one,1).
tr_number(two,2).
tr_number(three,3).
tr_number(four,4).
tr_number(five,5).
tr_number(six,6).
tr_number(seven,7).
tr_number(eight,8).
tr_number(nine,9).
tr_number(ten,10).

:- pred ag_number(integer, gram_number).
ag_number(1,sin).
ag_number(N,plu) :- N>1.

:- pred quantifier_pron(word_t, word_t, word_t).
quantifier_pron(everybody,every,person).
quantifier_pron(everyone,every,person).
quantifier_pron(everything,every,thing).
quantifier_pron(somebody,some,person).
quantifier_pron(someone,some,person).
quantifier_pron(something,some,thing).
quantifier_pron(anybody,any,person).
quantifier_pron(anyone,any,person).
quantifier_pron(anything,any,thing).
quantifier_pron(nobody,no,person).
quantifier_pron(nothing,no,thing).

:- pred prep(word_t).
prep(as).
prep(at).
prep(of).
prep(to).
prep(by).
prep(with).
prep(in).
prep(on).
prep(from).
prep(into).
prep(through).

:- pred noun_form(word_t, word_t, gram_number).
noun_form(Plu,Sin,plu) :- noun_plu(Plu,Sin).
noun_form(Sin,Sin,sin) :- noun_sin(Sin).
noun_form(proportion,proportion,_).
noun_form(percentage,percentage,_).

:- pred root_form(beta_t0).
root_form(1+sin).
root_form(2+_).
root_form(1+plu).
root_form(3+plu).

:- pred verb_root(word_t).
verb_root(be).
verb_root(have).
verb_root(do).
verb_root(border).
verb_root(contain).
verb_root(drain).
verb_root(exceed).
verb_root(flow).
verb_root(rise).

:- pred regular_pres(word_t).
regular_pres(have).
regular_pres(do).
regular_pres(rise).
regular_pres(border).
regular_pres(contain).
regular_pres(drain).
regular_pres(exceed).
regular_pres(flow).

:- pred regular_past(word_t, word_t).
regular_past(had,have).
regular_past(bordered,border).
regular_past(contained,contain).
regular_past(drained,drain).
regular_past(exceeded,exceed).
regular_past(flowed,flow).

:- pred rel_pron(word_t, unk_t3).
rel_pron(who,subj).
rel_pron(whom,compl).
rel_pron(which,undef).

:- pred poss_pron(word_t, gram_gender, integer, gram_number).
poss_pron(my,_,1,sin).
poss_pron(your,_,2,_).
poss_pron(his,masc,3,sin).
poss_pron(her,fem,3,sin).
poss_pron(its,neut,3,sin).
poss_pron(our,_,1,plu).
poss_pron(their,_,3,plu).

:- pred pers_pron(word_t, gram_gender, integer, gram_number, unk_t3).
pers_pron(i,_,1,sin,subj).
pers_pron(you,_,2,_,_).
pers_pron(he,masc,3,sin,subj).
pers_pron(she,fem,3,sin,subj).
pers_pron(it,neut,3,sin,_).
pers_pron(we,_,1,plu,subj).
pers_pron(them,_,3,plu,subj).
pers_pron(me,_,1,sin,compl(_)).
pers_pron(him,masc,3,sin,compl(_)).
pers_pron(her,fem,3,sin,compl(_)).
pers_pron(us,_,1,plu,compl(_)).
pers_pron(them,_,3,plu,compl(_)).

:- pred terminator(word_t, word_t).
terminator(.,_).
terminator(?,?).
terminator(!,!).

:- pred name(word_t).
name(_).

% ===========================================================================

% specialised dictionary

:- pred loc_pred(word_t, list(word_t)).
loc_pred(east,prep(eastof)).
loc_pred(west,prep(westof)).
loc_pred(north,prep(northof)).
loc_pred(south,prep(southof)).

:- pred adj(word_t, unk_t8).
adj(minimum,restr).
adj(maximum,restr).
adj(average,restr).
adj(total,restr).
adj(african,restr).
adj(american,restr).
adj(asian,restr).
adj(european,restr).
adj(great,quant).
adj(big,quant).
adj(small,quant).
adj(large,quant).
adj(old,quant).
adj(new,quant).
adj(populous,quant).

:- pred rel_adj(word_t, word_t).
rel_adj(greater,great).
rel_adj(less,small).
rel_adj(bigger,big).
rel_adj(smaller,small).
rel_adj(larger,large).
rel_adj(older,old).
rel_adj(newer,new).

:- pred sup_adj(word_t, word_t).
sup_adj(biggest,big).
sup_adj(smallest,small).
sup_adj(largest,large).
sup_adj(oldest,old).
sup_adj(newest,new).

:- pred noun_sin(word_t).
noun_sin(average).
noun_sin(total).
noun_sin(sum).
noun_sin(degree).
noun_sin(sqmile).
noun_sin(ksqmile).
noun_sin(thousand).
noun_sin(million).
noun_sin(time).
noun_sin(place).
noun_sin(area).
noun_sin(capital).
noun_sin(city).
noun_sin(continent).
noun_sin(country).
noun_sin(latitude).
noun_sin(longitude).
noun_sin(ocean).
noun_sin(person).
noun_sin(population).
noun_sin(region).
noun_sin(river).
noun_sin(sea).
noun_sin(seamass).
noun_sin(number).

:- pred noun_plu(word_t, word_t).
noun_plu(averages,average).
noun_plu(totals,total).
noun_plu(sums,sum).
noun_plu(degrees,degree).
noun_plu(sqmiles,sqmile).
noun_plu(ksqmiles,ksqmile).
noun_plu(million,million).
noun_plu(thousand,thousand).
noun_plu(times,time).
noun_plu(places,place).
noun_plu(areas,area).
noun_plu(capitals,capital).
noun_plu(cities,city).
noun_plu(continents,continent).
noun_plu(countries,country).
noun_plu(latitudes,latitude).
noun_plu(longitudes,longitude).
noun_plu(oceans,ocean).
noun_plu(persons,person).  noun_plu(people,person).
noun_plu(populations,population).
noun_plu(regions,region).
noun_plu(rivers,river).
noun_plu(seas,sea).
noun_plu(seamasses,seamass).
noun_plu(numbers,number).

:- pred verb_form(word_t, word_t, delta_t0, beta_t0).
verb_form(V,V,inf,_) :- verb_root(V).
verb_form(V,V,pres+fin,Agmt) :-
   regular_pres(V),
   root_form(Agmt),
   verb_root(V).
verb_form(Past,Root,past+_,_) :-
   regular_past(Past,Root).

verb_form(am,be,pres+fin,1+sin).
verb_form(are,be,pres+fin,2+sin).
verb_form(is,be,pres+fin,3+sin).
verb_form(are,be,pres+fin,_+plu).
verb_form(was,be,past+fin,1+sin).
verb_form(were,be,past+fin,2+sin).
verb_form(was,be,past+fin,3+sin).
verb_form(were,be,past+fin,_+plu).
verb_form(been,be,past+part,_).
verb_form(being,be,pres+part,_).
verb_form(has,have,pres+fin,3+sin).
verb_form(having,have,pres+part,_).
verb_form(does,do,pres+fin,3+sin).
verb_form(did,do,past+fin,_).
verb_form(doing,do,pres+part,_).
verb_form(done,do,past+part,_).
verb_form(flows,flow,pres+fin,3+sin).
verb_form(flowing,flow,pres+part,_).
verb_form(rises,rise,pres+fin,3+sin).
verb_form(rose,rise,past+fin,_).
verb_form(risen,rise,past+part,_).
verb_form(borders,border,pres+fin,3+sin).
verb_form(bordering,border,pres+part,_).
verb_form(contains,contain,pres+fin,3+sin).
verb_form(containing,contain,pres+part,_).
verb_form(drains,drain,pres+fin,3+sin).
verb_form(draining,drain,pres+part,_).
verb_form(exceeds,exceed,pres+fin,3+sin).
verb_form(exceeding,exceed,pres+part,_).

:- pred verb_type(word_t, unk_t5).
verb_type(have,aux+have).
verb_type(be,aux+be).
verb_type(do,aux+ditrans).
verb_type(rise,main+intrans).
verb_type(border,main+trans).
verb_type(contain,main+trans).
verb_type(drain,main+intrans).
verb_type(exceed,main+trans).
verb_type(flow,main+intrans).

:- pred adverb(word_t).
adverb(yesterday).
adverb(tomorrow).
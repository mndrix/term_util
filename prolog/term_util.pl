:- module(term_util, [ control/1
                     , map_args/3
                     ]).
:- use_module(library(delay)).

%%  control(+Term) is semidet.
%
%   True if Term is a control structure such as `,` or `;` etc.
control((_,_)).
control((_;_)).
control((_->_)).
control((_*->_)).
control(\+(_)).


%%  map_args(+Goal, ?Term1, ?Term2) is semidet
%
%   True if call(Goal, Term1Arg, Term2Arg) is true for each
%   corresponding argument of Term1 and Term2.  For example,
%
%       ?- map_args(succ, a(1,2,3), a(2,3,4)).
%       true.
:- meta_predicate term_util:map_args(2,?,?).
map_args(Goal, Term1, Term2) :-
    delay(univ(Term1, Functor, Args1)),
    delay(univ(Term2, Functor, Args2)),
    maplist(Goal, Args1, Args2).

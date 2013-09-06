:- module(term_util, [ control/1
                     , map_args/3
                     ]).

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
map_args(Goal, Term1, Term2) :-
    lazy_univ(Term1, Functor, Args1),
    lazy_univ(Term2, Functor, Args2),
    maplist(Goal, Args1, Args2).


% like =../2 but requires no instantiation
% belongs in library(delay)
lazy_univ(Term, Name, Args) :-
    var(Term),
    !,
    when(
        ( nonvar(Name), nonvar(Args) ),
        when_proper_list(Args, Term=..[Name|Args])
    ).
lazy_univ(Term, Name, Args) :-
    % nonvar(Term)
    Term =.. [Name|Args].

% belongs in library(delay)
when_proper_list(List, Goal) :-
    var(List),
    !,
    when(nonvar(List), when_proper_list(List, Goal)).
when_proper_list([], Goal) :-
    call(Goal).
when_proper_list([_|T], Goal) :-
    when_proper_list(T, Goal).

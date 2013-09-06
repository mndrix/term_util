:- module(term_util, [ control/1
                     , map_args/3
                     , simplify/2
                     ]).
:- use_module(library(apply), [maplist/2]).
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


%%  simplify(+Goal0, -Goal) is det
%
%   True if Goal0 can be simplified to Goal. Goal0 is typically a
%   clause body including conjunctions and disjunctions. By evaluating
%   goals whose value is statically known, and pruning unreachable
%   branches in control structures, we produce a simpler Goal.
%
%   For example, the following are all true:
%
%       simplify((true,fail,foo),fail).
%       simplify((X=a,foo(X)), foo(a)).
%       simplify((true->foo;bar), foo).
%       simplify((foo,true), foo).
simplify(Goal0, Goal) :-
    eval(Goal0, Goal1),
    !,
    simplify(Goal1, Goal).
simplify(Goal, Goal).


%%  eval(Goal0, Goal) is semidet
%
%   Perform a single static evaluation step. Called repeatedly by
%   simplify/2 until a fixed point is reached

% rules about disjunction
eval((fail;Others),Others).  % fail is a disjunctive noop

% rules about ->
eval((true->Then;_Else), Then). % unreachable Else
eval((fail->_Then;Else), Else). % unreachable Then
eval((If->Then;fail), (If->Then)). % Else fails by default
eval((If->true), once(If)). % using -> as a local cut (aka once/1)
eval((If0->Then;Else), (If->Then;Else)) :-
    % evaluable If
    eval(If0, If).
eval((If->Then0;Else), (If->Then;Else)) :-
    % evaluable Then
    eval(Then0, Then).
eval((If->Then;Else0), (If->Then;Else)) :-
    % evaluable If
    eval(Else0, Else).

% rules about conjunction
eval((true,Cont),Cont). % true is conjunctive noop
eval((Goal,true),Goal). % "
eval((fail,_), fail).  % failure removes continuation
eval((X=Y,Cont),Cont) :-
    % perform unification
    X = Y.
eval((A,Cont), (B,Cont)) :-  % eval a conjunctive goal
    eval(A,B).
eval((A,Cont0),(A,Cont)) :-  % recursively eval conjunction of goals
    eval(Cont0, Cont).

% goals that can be safely evaluated at compile time
eval(call(Goal),Goal) :-
    % call/1 not needed
    nonvar(Goal).
eval(Goal,Truth) :-
    safe_semidet(Goal),
    ( call(Goal) ->
        Truth = true
    ; % otherwise ->
        Truth = fail
    ).


%%  safe_semidet(Goal) is semidet.
%
%   True if Goal can be safely evaluated. The caller can replace it with
%   either `true` or `fail`.
safe_semidet(atom_codes(A,C)) :-
    ( ground(A) ; ground(C) ).
safe_semidet(Goal) :-
    Goal =.. [Name|Args],
    safe_semidet_nonvar(Name),
    maplist(nonvar, Args).

safe_semidet_nonvar(nonvar).
safe_semidet_nonvar(integer).
safe_semidet_nonvar(atom).
safe_semidet_nonvar(float).

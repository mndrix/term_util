:- use_module(library(term_util), [map_args/3]).

% define helper predicates here

:- use_module(library(tap)).

declare :-
    map_args(succ, a(1,2,3), a(2,3,4)).

forward :-
    map_args(succ, a(1,2,3), T),
    T == a(2,3,4).

backward :-
    map_args(succ, T, a(4,5,6)),
    T == a(3,4,5).

mixed :-
    map_args(succ, a(1,Two,3), a(2,3,Four)),
    Two =:= 2,
    Four =:= 4.

:- use_module(library(term_util), [control/1]).

:- use_module(library(tap)).

control((a,b)).
control((a;b)).
control((a->b)).
control((a*->b)).
control((\+a)).

:- use_module(library(term_util), [simplify/2]).

:- use_module(library(tap)).

% examples from the documentation
simplify((true,fail,foo),fail).
simplify((X=a,foo(X)), foo(a)).
simplify((true->foo;bar), foo).
simplify((foo,true), foo).


% a more complicated example
simplify(
    (
        true,
        a,
        X=b,
        call(X),
        ( true -> c; nope ),
        ( fail -> nope; d ),
        ( fail ; e ),
        foo(f) = foo(Y),
        call(Y),
        nonvar([_]),
        ( integer(9) -> g ; nope ),
        atom(42.0),  % fail
        nope
    ),
    (
        a,
        b,
        c,
        d,
        e,
        f,
        g,
        fail
     )
).

% simplification inside If->Then;Else
simplify(
    ( atom(hi) -> integer(9) ; nope ),
    true
).
simplify(
    ( atom(X) -> integer(9) ; float(hi) ),
    once(atom(X))
).

% some predicates that can be evaluated statically
simplify(
    ( atom_codes(A,[0'h, 0'i]), write(A) ),
    write(hi)
).
simplify(
    ( atom_codes(hi,C), write(C) ),
    write([0'h, 0'i])
).
simplify(  % cant' simplify this one
    ( atom_codes(A,C), write(A), write(C) ),
    ( atom_codes(A,C), write(A), write(C) )
).

% Cactus Environment Machine
% George Stelle 

# Introduction

It has become standard for compilers of lazy functional langauges to implement
*graph reduction* of *supercombinators* \cite{jonesstg,
Johnsson85lambdalifting}. This allows for functions calls to be compiled similar
to imperative languages, with arguments moved into registers before entering the
code. This approach is capable of generating very fast code, but it comes with
costs. One such cost is the increased cost of thunks. To understand this cost,
consider the following expression:

    f w x y z g = let m = (\v -> w v x y z) n in g m

To lamba lift this, we name the lambda in the let binding and add `w x y z`
as arguments. Then, as it is now a supercombinator, we can move it to global
scope. The code is now transormed to:

    f w x y z g = let m = fresh w x y z n in g m
    fresh w x y z v = w v x y z

In this code, because we cannot a-priori know the strictness of `g`, `m` is
created as an unevaluated computatio, or *thunk*. The key idea is that because
we made all of the free variables in our anonymous function arguments, we must
copy all of them into the heap. You can imagine a worse scenario, where multiple
thunks all using multiple arguments of `f` are created, duplicating each of
`f`'s arguments multiple times.

There is a sort of fuzzy, high level philosophical view one can take of this
class of evaluation.

Along with some more subtle 




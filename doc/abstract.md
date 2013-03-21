% Cactus Environment Machine
% George Stelle 

# Abstract

Traditionally, functional languages are compiled using graph reduction of
supercombinators. This is achieved by lambda lifting terms so that there are no
free variables. While this allows efficient compilation, it creates closures on
the heap during application, as well as duplicating any shared environment. This
complicates updates and creates unnecessary thunks on application.  We propose a
new technique for lazy evaluation using a *Cactus Environment Machine* (CEM).
The cactus environment is a novel approach to representing the context of a
computation, allowing efficient evaluation, lazy updates without indirections,
and reductions in heap size. We also show that a simple virtual machine
implementation has performance similar to mature implementations.


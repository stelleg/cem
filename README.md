% Cactus Environment Machine

# Configuring
Edit `cemDir` variable in `Main.hs` to point to build directory.  Needed for
use of assembly macros.

# Building
    make

# Installing
    cabal install
or 
    make install

Make sure ~/.cabal/bin/ is in your PATH variable.

# Usage
    cem {options} file.lc

# Examples
    cem test/hello.lc # compiles hello world program in test/
    cem -g test/noprelude/fib.lc
    cem -ld test/hello.lc

# Flags
`-l` Put before any other flag, uses the libraries: `lib/prelude.lc` `lib/os.lc` `lib/church.lc`
`-t`: Trace. Dumps term for every machine step. Useful for debugging.
`-r`: Runs in virtual machine.
`-g`: Visualization tool. Also probably want to use without libs.
`-a`: Controlf low analysis (0CFA). Very slow, probably want to use without `-l`
`-f`: Free variable analysis. 

# Syntax
See `lc` and `sugar` from `IO.hs` for parser syntax. Essentially a list of let
bindings followed by an `lc` expression.

# Required haskell libraries
- graphviz
- parsec
- probably others

# Issues
- VM doesn't have argc and argv implemented
- GC doesn't grow heap when needed, default large (400M)
- `writeFd` and `readFd` max out at a single page size, to read or write more
  must use multiple calls

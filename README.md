% Cactus Environment Machine

# Configuring
Edit `cemDir` variable in `Main.hs` to point to build directory.  Needed for
use of assembly macros.

# Building
    make

# Usage
    cem {options} test/testfile.lc

# Examples
    cem test/hello.lc # compiles hello world program in test/
    cem -g test/noprelude/fib.lc
    cem -ld test/hello.lc

# Flags
`-l` before anything uses the libraries: `lib/prelude.lc` `lib/os.lc` `lib/church.lc`
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
- No syntax support for nested lets

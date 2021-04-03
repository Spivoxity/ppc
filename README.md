# ppc
Multi-target picoPascal compiler

This compiler implements the picoPascal language of my Compilers course on multiple targets.  
It's mostly an exercise in using the OCaml module system to construct the compiler from separate target-dependent and
target-independent pieces, so that the two parts meet only at the last minute with (for instance) the last line of `arm.ml`
reading
````
module Compiler = Main.F(ARM)
````
Back ends are provided for Native ARM, for ARM Thumb, for AMD64 with the Linux calling convention, for MIPS, and for Risc86, a cosmetic variant of the x86
with a RISC-like instruction set.

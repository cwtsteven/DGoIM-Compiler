# 1 Description
This is a compiler based on the Dynamic Geometry of Interaction machine built for lambda calculus with numerals and arithmetic operations. It compiles the source code into self-modifying x86 assembly code which captures the rewrite transitions in DGoIM. 

# 2 Dependencies
In order to compile the project correctly, please make sure you have the following installed.

1. Ocaml, minimum version 4.05.1
2. Menhir, minimum version 20151005

# 3 How to compile
To compile simply execute <code>make</code>.

# 4 How to use the compiler
1. To use the compiler, execute <code>./Main.native [src]</code>. For example: <code>./Main.native main.txt</code>. The compiler will generate an assembly file in the same directory. 
2. Run `gcc <src>`. 
3. Run `./a.out` to execute the program. 

# 5 Syntax
```
<term> ::= <var>
         | <const> 
         | fun x -> <term> 
         | <term> <term>
         | <term> + <term>
```

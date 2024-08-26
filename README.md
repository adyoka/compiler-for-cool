# Compiler for COOL

This is a compiler for a programming language called COOL. 

The compiler has 4 compiling phases: lexical analysis, parsing, semnatic analyis, and code generation. The last phase compiles into a MIPS assembly. 

### Overview

COOL (Classroom Object Oriented Language) is a computer programming language designed by Alexander Aiken for use in learning to implement simple complete compilers in compiler courses.

COOL resembles modern programming languages, such as Java, and features Object-Oriented programming, memory management, and static types. 

The compiler targets MIPS 32-bit assembly. [MIPS](https://en.wikipedia.org/wiki/MIPS_architecture) is a reduced instruction set architecture. It is small and easy to learn, hence, is used as the target ISA for this project. The compiled assembly code is run on a [SPIM simulator](http://spimsimulator.sourceforge.net/).

An example of a COOL program:

```
class Main inherits IO {
   main(): SELF_TYPE {
	out_string("Hello, World.\n")
   };
};
```

Resources: [COOL Manual](http://theory.stanford.edu/~aiken/software/cool/cool-manual.pdf)

This projects follow the structure of Stanford [course](https://online.stanford.edu/courses/soe-ycscs1-compilers).

### Project Structure
 * [Lexer](./src/Lexer/) - implements the lexical analysis and tokenizes the input program
 	* Written in flex specifications, which generates a lexer
 * [Parser](./src/Parser/) - implements a LALR(1) parser that given the tokens constructs an abstract syntax tree
  	* Written in bison specificatiosn, which generates a parser
	* Simple syntax directed translation
 * [Semantic Analyser](./src/SemAnalyser/) - implements a pass that does semantical analysis, mainly type checking
 	* Written in C++, takes an abstract syntax tree generated from parser and outputs attributed abstract syntax tree
 * [Code Generator](./src/CodeGenerator/) - implements a pass for code generation using a naive treewalk algorithm, generating MIPS assembly)
 	* Written in C++, takes the attributed abstract syntax tree and outputs a file of MIPS instructions

### Dependencies
You need to install:

* g++
* bison
* flex
* spim

Better to set-up in 32-bit Linux on VM.
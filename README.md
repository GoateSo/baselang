# Baselang.scala

A minimal implementation of the Base language compiler for [UW Madison CS536](https://pages.cs.wisc.edu/~hasti/cs536/) in scala. 

The parsing stage was implmented using [FastParse](https://com-lihaoyi.github.io/fastparse/), and the remaining stages were implemented manually, using the java program as a guide. 

The compiler implementation explores a mixture of paradigms, leveraging functional parser combinators during parsing and recursive tree exploration with imperative state management during name analysis, type checking, and codegen. 

Overall, the project (as of 5/23/24) consists of 1083 lines of code spread over 6 main files, `AST.scala(144)`,`Codegen.scala(339)`,`Main.scala(36)`,`NameAnalyzer.scala(188)`,`Parser.scala(185)`, and `TypeChecker.scala(191)` compared to the ~3000 line megalith of an `ast.java` file in the original project (not counting any smaller helper files/ files for parser/lexer generation). 

## Specifications

This project attempts to keep as close to the original as possible, where:
- compilation stages in order are:
    1. parsing : input string -> ast
    2. name analysis : ast -> list of name resolution errors
    3. type checking : ast -> list of type errors
    4. code generation : ast -> bytecode
- both the name analysis and type checking phases result in lists of errors, as opposed to single errors
- the output language is MIPS assembly, specifically targeted for [QTSpim](https://spimsimulator.sourceforge.net/).

But deviates from the original specs in some cases:
- strings aren't as expessive -- this is just to make parsing them more simple instead of being a regex mess
- the lexical analysis step was skipped in favor of directly parsing to AST -- design choice owing to using FastParse. This certainly made handling comments (removed by lexer) and location data (assigned to each token) a bit cumbersome, but overall was not that big a change
- error messages are changed from the original -- not particularly worth matching up; the specified errors aren't particularly terse or informative, using FastParse as my parser combinator makes matching syntax errors a hassle.
- spacing in the generated code isn't exactly the same -- didnt have the heart to wholly copy the provided codegen helper class.


## To be completed

- adding proper code generation for tuples (this was not a requirement for the course)
- making a proper REPL (most likely an AST walking interpreter) instead of having a faux loop that just serves as a frontend for compilation
- fixing any bugs that arise. Although I have tested against my existing code, those tests certainly aren't thorough enough to catch all bugs present.
- optimizing the code produced by the compiler
- extending the language with additional features (from languages like scala or lua) like
    - arrays
    - additional ctrl structures (break, continue, for loops, switch/match exprs/stmts)
    - more expressivity for functions (taking tuple params/ returning tuples, fwd decls)
    - type inference 
    - allowing nested / first class functions
    - a standard library

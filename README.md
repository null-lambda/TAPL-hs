
# TAPL-hs
Example codes of TAPL, rewritten in haskell for personal study.

[TAPL book](http://www.cis.upenn.edu/~bcpierce/tapl/main.html) \- B. C. Pierce, *Types and Programming Languages*, Cambridge, MA, MIT Press, 2002. 

[Original codes (OCaml version)](http://www.cis.upenn.edu/~bcpierce/tapl/checkers/)


## Projects

* *[arith](arith)* (Ch. 3 - 4) - Arithmetic Expressions (**NB**)  
* *[untyped](untyped)* (Ch. 5 - 7) - Lambda-Calculus (**λ**)   
* ~~*fulluntyped* (Ch. 5 - 7) - **λ** + **NB**~~  
* *[tyarith](tyarith)* (Ch. 8) - Typed Arithmetic Expressions (**NB**)   
* *[simplebool](simplebool)* (Ch. 10) - Simply Typed Lambda Calculus (**λ<sub>→</sub>**) with Booleans     
* *[fullsimple](fullsimple)* (Ch. 9 - 11) - Extensions **λ<sub>→</sub>**   
natural, boolean, float, string, unit,   
type variables, abbreviated terms and types,
let-in, fix, ~~ascription~~, record, variants, case-of



## Running 
* install stack or haskell platform 


* run in ghci interpreter
```bash
$ cd <project-dir>
$ ghci    # stack exec -- ghci 
Prelude> :l Main.hs
Main> :set args <testfile>  # <testfile> is located in examples/..
Main> main 
```

* compiled version
```bash
$ stack build 
$ stack exec -- <project> <testfile>
```

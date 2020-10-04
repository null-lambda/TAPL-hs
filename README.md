# TAPL-hs
Example codes of TAPL, rewritten in haskell for personal study.

[TAPL book](http://www.cis.upenn.edu/~bcpierce/tapl/main.html) \- B. C. Pierce, *Types and Programming Languages*, Cambridge, MA, MIT Press, 2002.  

```
# run code in ghci 
stack exec -- ghci 
:l ../<project>/Main.hs
:set args ../examples/<test file>.f 
main 
```

```
# compile & run
stack build 
stack exec -- <project> <test file>.f
```



[Original examples codes, OCaml version](http://www.cis.upenn.edu/~bcpierce/tapl/checkers/)]

## Projects

*[arith](arith)* (Ch. 3 - 4) - Untyped Naturals and Booleans (**NB**) 
*[untyped](untyped)* (Ch. 5 - 7) - Untyped Lambda-Calculus (**λ**) 
~~*fulluntyped* (Ch. 5 - 7) - **λ** + **NB**~~
*[tyarith](tyarith)* (Ch. 8) - Typed Arithmetic Expressions (**NB**) 
*[simplebool](simplebool)* (Ch. 10) - Simply Typed Lambda Calculus (**λ<sub>→</sub>**) with Booleans 
*[fullsimple](fullsimple)* (Ch. 9 - 11) - Extended **λ<sub>→</sub>** 
> natural, boolean, float, string, unit, 
> let bindings, ~~ascription~~, ~~fixpoint~~,
> ~~record, variants~~



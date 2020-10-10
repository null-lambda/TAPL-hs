
# TAPL-hs
Example codes of TAPL, rewritten in haskell for personal study.

[TAPL book](http://www.cis.upenn.edu/~bcpierce/tapl/main.html) \- B. C. Pierce, *Types and Programming Languages*, Cambridge, MA, MIT Press, 2002. 

[Original codes (OCaml version)](http://www.cis.upenn.edu/~bcpierce/tapl/checkers/)


## Projects

* *[arith](arith)* (Ch. 3, 4) - Arithmetic Expressions (**NB**)  

```
    t   = true | false | if t then t else t     (booleans)
        | zero | succ t | pred t | iszero t     (naturals)
```
* *[untyped](untyped)* (Ch. 5, 6, 7) - Lambda-Calculus (**λ**)   
```
    t   = x             (variables) 
        | λ x . t       (abstraction) 
        | t t           (applicaiton)
```
* *[tyarith](tyarith)* (Ch. 8) - Typed Arithmetic Expressions (**NB**) 
```
    t   = (...)         
    T   = Bool | Nat    (atomic types)
```
* *[simplebool](simplebool)* (Ch. 10) - Simply Typed Lambda Calculus (**λ<sub>→</sub>**) with Booleans     
```
    t   = (...)        
        | λ x:T. t          (typed abstraction)

    T   = (...)         
        | T -> T            (arrow)
```
* *[fullsimple](fullsimple)* (Ch. 9, 10, 11) - Extensions of **λ<sub>→</sub>**  
```
    t   = A | (...)                                     (literal, variables, operations)
        | ()                                            (unit)
        | t as T                                        (ascription)
        | let x=t in t                                  (let binding)
        | fix | letrec x=t in t                         (recursion)
        | {t1, ...} | {l1:t1, ...} | t.l                (tuple, record) 
        | <li:ti> as T | case t of <l1=x1> => t1 ...    (variant)
        | λ _:T. t | <l=_>                              (wildcard)
        | (t1; ...)                                     (sequencing

    T   = A' | (...)                                    (base type, type variables)
        | ()                                            (unit type)
        | {T1, ...} | {l1:T1, ...}                      (product type)
        | <l1:T1, ...>                                  (sum type)
``` 
* *[fullref](fullref)* (Ch. 13, 18) - fullsimple + References with heap memory (store)
```
    t   = (...)
        | ref t             (reference creation)
        | !t                (dereference)
        | t := t            (assignment)
        | l                 (store location)

    T   = (...)
        | Ref T 
```

## Running 
* Install stack or haskell platform 

* Run in ghci interpreter
```bash
$ cd <project-dir>
$ ghci                      # stack exec -- ghci 
Prelude> :l Main.hs
Main> :set args <testfile>  # <testfile> is located in examples/..
Main> main 
```

* Compiled version
```bash
$ stack build 
$ stack exec -- <project> <testfile>
```

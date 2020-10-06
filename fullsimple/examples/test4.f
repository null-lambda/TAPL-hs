

equal = fix (
    \eq:Nat->Nat->Bool m:Nat n:Nat.
        if iszero m
            then iszero n 
        else if iszero n
            then false 
        else 
            eq (pred m) (pred n)
    );

OptionalNat = <none:Unit, some:Nat>;
Table = Nat -> OptionalNat;
emptyTable =\n:Nat . <none=()> as OptionalNat: Table;
extendTable = \t:Table m:Nat v:Nat n:Nat.
    if equal n m 
        then <some=v> as OptionalNat
        else t n 
    : Table -> Nat -> Nat -> Table;

emptyTable 10;
ta = extendTable emptyTable 10 5;
ta 10; 

letrec fib:Table = (
    letrec plus : Nat->Nat->Nat = 
        λ m:Nat n:Nat.
            if iszero m
            then n 
            else plus (pred m) (succ n) 
        in
    let prd2 = λ n:Nat. pred (pred n) in
    let opt = λ n:OptionalNat. 
        case n of <some=m> => m | <none=_> => 0 in 
    λ n:Nat. 
        if iszero (pred n) then <some=1> as OptionalNat 
        else <some=plus (opt (fib (pred n))) (opt (fib (prd2 n)))> as OptionalNat
    ) in
    let f = λn:Nat. {n=n, fibn=fib n} in 
    {f 2, f 8, f 10};

// exercise 11.2.1
u=(); U=(); UU=()-> ();
t = fix (\t:Nat->U. \n:Nat. 
    if iszero n 
        then (\x:U. x) u
        else (\f:UU. f(f(u))) (\x:U. t(pred n))

    );

t 2; 
(t 15; t 2);

/*
NatList = <nil:Unit, cons:{Nat,NatList}>;
nil = <nil=unit> as NatList;
cons = lambda n:Nat. lambda l:NatList. <cons={n,l}> as NatList;
*/
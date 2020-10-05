

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

fib =
    let plus = fix (
    \f:Nat->Nat->Nat m:Nat n:Nat.
        if iszero m
        then n 
        else f (pred m) (succ n)
    ) in
    let prd2 = \n:Nat. pred (pred n) in
    let opt = \n:OptionalNat. 
        case n of <some=m> => m | <none=l> => 0 in 
    fix (\t:Table n:Nat. 
        if iszero (pred n) then <some=1> as OptionalNat 
        else <some=plus (opt (t (pred n))) (opt (t (prd2 n)))> as OptionalNat
    );
fib 10;

/*
NatList = <nil:Unit, cons:{Nat,NatList}>;
nil = <nil=unit> as NatList;
cons = lambda n:Nat. lambda l:NatList. <cons={n,l}> as NatList;
*/
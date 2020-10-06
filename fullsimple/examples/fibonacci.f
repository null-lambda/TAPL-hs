
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
/*
NatList = <nil:Unit, cons:{Nat,NatList}>;
nil = <nil=unit> as NatList;
cons = lambda n:Nat. lambda l:NatList. <cons={n,l}> as NatList;
*/
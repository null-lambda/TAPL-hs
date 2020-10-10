

equal = fix (
    \eq:Nat->Nat->Bool m:Nat n:Nat.
        if iszero m
            then iszero n 
        else if iszero n
            then false 
        else 
            eq (pred m) (pred n)
    );
    
c = ref 0;
incc = \x:Unit. (c := succ (!c); !c);
decc = \x:Unit. (c := pred (!c); !c);
o = {i = incc, d = decc};
!c; 
{o.i (), o.i (), o.i (), o.d()};
!c;


NatArray = Ref (Nat->Nat);
newarray = \_:(). ref (\n:Nat.0);
lookup = \a:NatArray. \n:Nat. (!a) n;
update = \a:NatArray. \m:Nat. \v:Nat.
    let oldf = !a in
    a := (λn:Nat. if equal m n then v else oldf n);

arr = newarray ();
updatedArr = (update arr 10 7; update arr 5 10;  !arr);
{updatedArr 3, updatedArr 5, updatedArr 10};

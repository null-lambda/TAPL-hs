
 lambda x:<a:Bool,b:Bool>. x;


{x=true, y=false};
{x=true, y=false}.x;
{true, false};
{true, false}.1;


plus =
  fix (lambda f:Nat->Nat->Nat. lambda a:Nat. lambda b:Nat.
         if iszero b then a else f (succ a) (pred b));

plus 10 0;
plus 5 6;
plus 0 12;

/*
NatList = <nil:Unit, cons:{Nat,NatList}>;
nil = <nil=unit> as NatList;
cons = lambda n:Nat. lambda l:NatList. <cons={n,l}> as NatList;
*/

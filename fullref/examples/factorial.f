plus = fix (
  \f:Nat->Nat->Nat m:Nat n:Nat.
    if iszero m
      then n 
      else f (pred m) (succ n)
  );

times = fix (
  \f:Nat->Nat->Nat. \m:Nat. \n:Nat.
    if iszero m 
      then 0
      else plus n (f (pred m) n)
  );

factorial = fix (
  \f:Nat->Nat m:Nat.
    if iszero m 
      then 1 
      else times m (f (pred m))
  );

plus 10 0;
plus 5 6;
plus 0 12;

times 5 10;
factorial 10;
factorial 5;

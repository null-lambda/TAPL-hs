/* test 1. primitives */

"Hello, World!";

unit;
lambda x:Nat. succ x;
(lambda x:Nat. succ (succ x)) (succ 0);
timesfloat 2.0 3.14159;

A;
lambda x:A. 0.151523;

an:Nat;
bn = lambda x:Nat. iszero x;
bn an ;
bn 10;
\x:Nat->Bool y:Nat. x y;

T = Nat->Nat;
g : T ; 
(\z:Nat -> Nat . z 3) g;

lambda f:T. lambda x:Nat. f (f x);

a: Nat;
let f= (\b:Nat->Nat .(\a:Nat->Nat. b)) in f (\c:Nat. a);

let a = 10 
  in let b = 
    if iszero (pred (pred (pred(3))))
      then a 
      else 99
  in succ (b);

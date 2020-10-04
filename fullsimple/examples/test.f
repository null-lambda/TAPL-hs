/* Examples for testing */


let x=true in x;

unit;
lambda x:Nat. succ x;
(lambda x:Nat. succ (succ x)) (succ 0);

A;
lambda x:A. x;

an:Nat;
bn = lambda x:Nat. iszero x;
bn an ;
bn 10;
\x:Nat->Bool y:Nat. x y;
k =  \x:Nat. iszero (pred x);
k;
T = Nat->Nat;
g : T ; 
(\z:Nat -> Nat . z 3) g;

lambda f:T. lambda x:Nat. f (f x);

a: Nat;
(\b:Nat->Nat .(\a:Nat->Nat. b)) (\c:Nat. a);


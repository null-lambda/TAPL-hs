
A;
lambda x:A. 0.151523;

an : A; // term variable binding
an as A;
bn = lambda x:Nat. iszero x;  //  abbreviation
bn an ;
bn 10;
\x:Nat->Bool y:Nat. x y;

T = Nat->Nat; // type variable binding with abbreviation
g : T ; 
(\z:Nat -> Nat . z 3) g;

(lambda f:T. lambda x:Nat. f (f x)) as T -> Nat -> Nat;

a : Nat;
let f= (\b:Nat->Nat .(\a:Nat->Nat. b)) in f (\c:Nat. a);
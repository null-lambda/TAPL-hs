
lambda x:<a:Bool,b:Bool>. x;


unit;
lambda x:Nat. succ x;
(lambda x:Nat. succ (succ x)) (succ 0);
timesfloat 2.0 3.14159;

{x=true, y=false};
{x=true, y=false}.x;
{true, false};
{true, false}.1;
({{"00","11"},{"22","33"}}.2 as {String, String}).1;

PhysicalAddr = {firstlast:String, addr:String};
VirtualAddr= {name:String, email:String};
Addr = <physical:PhysicalAddr, virtual:VirtualAddr>;
pa = {firstlast="Mike", addr="Unknown"}: PhysicalAddr;
a = <physical= pa> as Addr;

getName = \a:Addr.
case a of
	<physical=x> => x.firstlast
	| <virtual=y> => y.name;
    a;
{hell= a, c=getName a};
getName a;


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

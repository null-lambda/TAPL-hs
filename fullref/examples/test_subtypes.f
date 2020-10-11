

 lambda x:Bot. x;
 lambda x:Bot. x x;


(\x:<a:Bool,b:Bool>. x) (<a=true> as <a:Bool>);


(\r:{x:Top->Top}. r.x r.x)
  {x=\z:Top.z, y=\z:Top.z};

OptionalNat = <none: (), some: Nat>;
func = \t:OptionalNat. case t of 
    <none=_> => {1, 2, {a=0}} 
  | <some=n> => {10, n, {a=10, b=iszero n}};
func (<some=3> as <some: Nat>);

a = ref 10; 
b = ref {node=a, data=23} as {node: Ref Nat, data: Nat};
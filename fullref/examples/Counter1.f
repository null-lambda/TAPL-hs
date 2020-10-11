

Counter = {get:()->Nat, inc:()->()};
newCounter = \_:(). 
  let x = ref 1 in
	{get = λ _:(). !x,
	 inc = λ _:(). x:=succ(!x)
    } as Counter;
inc3 = λc:Counter. (c.inc (); c.inc (); c.inc ());

c = newCounter ();
c.get ();
{c0=c.get (), 
 c1=(inc3 c; c.get ())
 };

ResetCounter = {get:()->Nat, inc:()->(), reset:()->()};
newResetCounter = \_:(). 
  let x = ref 1 in
	{get = \_:(). !x,
	 inc = \_:(). x:=succ(!x),   
     reset =\_:(). x:=1
    } as ResetCounter;
rc = newResetCounter ();
(inc3 rc; rc.reset (); inc3 rc; rc.get ());
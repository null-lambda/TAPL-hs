
// base class
CounterRep = {x: Ref Nat};
Counter = {get:()->Nat, inc:()->()};
counterClass = \r:CounterRep.
	{ get = \_:(). !r.x
	, inc = \_:(). r.x:=succ(!r.x)
  } as Counter;
newCounter = \_:(). 
  let r = {x=ref 1} 
  in counterClass r;

inc3 = λc:Counter. (c.inc (); c.inc (); c.inc ());

c = newCounter ();
c.get ();
{c0=c.get (), 
 c1=(inc3 c; c.get ())};

// sub class with new method
ResetCounter = {get:()->Nat, inc:()->(), reset:()->()};
resetCounterClass= \r:CounterRep. 
  let super = counterClass r in 
    { get = super.get 
    , inc = super.inc
    , reset = \_:(). r.x := 1
    } as ResetCounter;
newResetCounter = \_:().
  let r = {x=ref 1}
  in resetCounterClass r; 

rc = newResetCounter ();
(inc3 rc; rc.reset (); inc3 rc; rc.get());

// another sub class 
InverseCounter = {get:()->Nat, inc:()->(), dec:()->(), reset:()->()};
inverseCounterClass = \r:CounterRep.
  let super = resetCounterClass r in 
  { get = super.get 
  , inc = super.inc
  , dec = \_:(). r.x:=pred (!r.x)
  , reset = super.reset
  } as InverseCounter;
newInverseCounter = \_:().
  let r = {x=ref 1}
  in inverseCounterClass r;

ic = newInverseCounter ();
(inc3 ic; ic.dec (); ic.get ());

// sub class with new method and variable
BackupCounter = {get:Unit->Nat, inc:Unit->Unit, reset:Unit->Unit, backup: Unit->Unit};
BackupCounterRep = {x: Ref Nat, b: Ref Nat};
backupCounterClass = \r:BackupCounterRep.
  let super = resetCounterClass r in
    { get = super.get
    , inc = super.inc
    , reset = λ_:Unit. r.x:=!(r.b)
    , backup = λ_:Unit. r.b:=!(r.x)
    } as BackupCounter;
bc = backupCounterClass {x=ref 1, b=ref 0};
{ c0=(inc3 bc; bc.backup(); bc.get()), 
  c1=(bc.inc(); bc.get()),
  c2=(bc.reset(); bc.get())
};